{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Unsafe #-}
module Control.Monad.STE.Internal
(
  STE(..)
  ,unSTE
  ,STERep
  ,STEret(..)
  ,runSTE2
  ,runSTEither
  ,runSTE
  ,throwSTE
  ,handleSTE2
  ,handleSTE
  ----
  ,unsafeInterleaveSTE
  ,liftSTE
  ,fixSTE
  ,runBasicSTE
  ,RealWorld
  ,unsafeIOToSTE
  ,unsafeSTEToIO
  )

  where

import GHC.Prim (State#, raiseIO#, catch#)

import qualified Control.Monad.Catch as CMC
import Control.Exception as Except
import Control.Monad (ap)
import qualified Control.Monad.Fix as MF
import Control.Monad.Primitive
import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)
import GHC.IO(IO(..))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

#if MIN_VERSION_ghc_prim(0,5,0)
import GHC.Magic(runRW#)
#else
import GHC.Prim (realWorld#, Any)
#endif


newtype STE e s a = STE  (STERep s a)

unSTE :: STE e s a -> STERep s a
unSTE = \(STE a) ->  a

type STERep s a = State# s -> (# State# s, a #)

data STEret s a = STEret (State# s) a

-- liftSTE and STEret is useful when we want a lifted result from an STE computation.  See
-- fixSTE below.
liftSTE :: STE e s a -> State# s -> STEret s a
liftSTE (STE m) = \s -> case m s of (# s', r #) -> STEret s' r

{-# NOINLINE unsafeInterleaveSTE #-}
unsafeInterleaveSTE :: STE e s a -> STE e s a
unsafeInterleaveSTE (STE m) = STE ( \ s ->
    let
        r = case m s of (# _, res #) -> res
    in
    (# s, r #)
  )

-- | Allow the result of a state transformer computation to be used (lazily)
-- inside the computation.
-- Note that if @f@ is strict, @'fixSTE' f = _|_@.
fixSTE :: (a -> STE e s a) -> STE e s a
fixSTE k = STE $ \ s ->
    let ans       = liftSTE (k r) s
        STEret _ r = ans
    in
    case ans of STEret s' x -> (# s', x #)


instance Functor (STE e s) where
    fmap f (STE m) = STE $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Applicative (STE e s) where
    {-# INLINE pure  #-}
    {-# INLINE (<*>) #-}
    {-# INLINE (*> ) #-}
    pure = return
    (*>) = \ m k ->  m >>= \ _ -> k
    (<*>) = ap

instance Monad (STE e s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = STE (\ s -> (# s, x #))
    m >> k   = m >>= \ _ -> k
    (STE m) >>= k
      = STE (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { STE k2 ->
        (k2 new_s) }})

instance MF.MonadFix (STE e s) where
  mfix = fixSTE

instance PrimMonad (STE e s) where
  type PrimState (STE e s) = s
  primitive = \ m ->  STE m
  {-# INLINE primitive #-}
instance PrimBase (STE e s) where
  internal (STE p) = \ s# -> case p s# of
                          (# a , b #) -> (# a , b #)
  {-# INLINE internal #-}

-- this isn't terribly useful, but it is the only valid instance STE can have for
-- MonadThrow and be well behaved, though this may be replaced with
-- an
instance (Except.SomeException ~ err) =>  CMC.MonadThrow (STE err s) where
  throwM x = throwSTE  $ toException x


{-# INLINE runSTE #-} -- this may not be needed and may make code closer when its a small STE computation (though we're using it for small stuff )
-- | 'runSTE' is one way of interpreting the STE monad. Runs an STE
-- computation, and also does the toplevel handling of the abortive throwSTE
-- operator. 'runSTE' does not and cannot (by design) handle pure or async
-- exceptions.
runSTE ::  (forall s. STE e s a) -> (Either e a  -> b) -> b
runSTE st f = runSTE2 st (f . Left) (f . Right)

-- | 'runSTEither' is one way of interpreting the STE monad. Runs an STE
-- computation, and also does the toplevel handling of the abortive throwSTE
-- operator. 'runSTEither' does not and cannot (by design) handle pure or async
-- exceptions.
runSTEither :: (forall s. STE e s a) -> Either e a
runSTEither st = runSTE2 st Left Right

{-# INLINE handleSTE #-}
-- | 'handleSTE' is a flipped convenience function version of 'runSTE'
handleSTE :: (Either e a -> b) -> (forall s. STE e s a)  -> b
handleSTE f st = runSTE st f

-- |  'throwSTE' is the 'STE' sibling of 'throwIO', and its argument
-- must match the @e@ parameter in @'STE' e s a@. There is also no Exception e
-- constraint.
-- `throwSTE` should be thought of as an "abort" operation which is guaranteed to be
-- caught/handled by runSTE.
throwSTE :: forall e s a .  e -> STE e s a
throwSTE err = unsafeIOToSTE $
    IO (raiseIO# (toException $ STException (unsafeCoerce err)))

-- | 'runSTE2' is the workhorse of the STE monad. Runs an STE computation, and
-- also does the toplevel handling of the abortive throwSTE operator.
-- 'runSTE2' does not and cannot (by design) handle pure or async exceptions.
runSTE2 :: forall e a b. (forall s. STE e s a) -> (e -> b) -> (a -> b) -> b
runSTE2 st f g = runBasicSTE $ unsafeIOToSTE $
    IO (catch# (unsafeCoerce (fmap g st)) handler')
  where
    handler' :: SomeException -> STERep RealWorld b
    handler' e = case fromException e of
        Just (STException val) -> \s -> (# s, f (unsafeCoerce val) #)
        Nothing -> raiseIO# e

{-# INLINE handleSTE2 #-}
-- | 'handleSTE2' is a flipped convenience function version of 'runSTE2'
handleSTE2 :: (e -> b) -> (a -> b) -> (forall s. STE e s a) -> b
handleSTE2 f g st = runSTE2 st f g

{-
catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)

catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raiseIO# e


see https://phabricator.haskell.org/D1973 and
https://ghc.haskell.org/trac/ghc/ticket/11555
for information on tradeoffs in strictness

-}


unsafeIOToSTE        :: IO a -> STE e s a
unsafeIOToSTE (IO io) = STE $ \ s -> unsafeCoerce io s

unsafeSTEToIO :: STE e s a -> IO a
unsafeSTEToIO (STE m) = IO (unsafeCoerce m)


--- this results in WAY better perf when available
#if MIN_VERSION_ghc_prim(0,5,0)
runBasicSTE :: (forall s. STE e s a) -> a
runBasicSTE (STE st_rep) = case runRW# st_rep of (# _, a #) -> a
{-# INLINE runBasicSTE #-}
#else
runBasicSTE :: (forall s. STE e s a) -> a
runBasicSTE st = runSTERep (case st of { STE st_rep -> st_rep })
{-# INLINE runBasicSTE #-}

runSTERep :: (forall s. STERep  s a) -> a
runSTERep st_rep = case st_rep realWorld# of
                        (# _, r #) -> r
{-# NOINLINE runSTERep #-}
#endif

#if MIN_VERSION_ghc_prim(0,5,0)
type family Any where {}
#endif

-- | STException
newtype STException = STException Any deriving Typeable
instance Show STException where
  show _ = "STException(..)! did you use the Unsafe/internal STE interface?"
instance Exception STException
