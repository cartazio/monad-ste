module Main where
import Test.Hspec
--import Control.Exception
import Control.Monad.STE

main ::  IO ()
main = hspec  spec

spec :: Spec
spec = describe "STE Spec " $ do
  it "catches errors" $
    Left "some error" == handleSTE id (do  _ <- throwSTE "some error"; return (1 :: Int ))
  it "returns stuff" $
    (1 :: Int) == (either (error "fail") id $ handleSTE id (return (1 :: Int )))
