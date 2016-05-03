#!/usr/bin/env runghc

-- | New-style @.travis.yml@ script generator using cabal 1.24's nix-style
-- tech-preview facilities.
--
-- See also <https://github.com/hvr/multi-ghc-travis>
--
-- NB: This code deliberately avoids relying on non-standard packages
--     is expected to compile/work with at least GHC 7.0 through GHC 8.0

import Control.Monad
import Data.List
import System.Environment
import System.Exit
import System.IO

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription (packageDescription, testedWith)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text
import Distribution.Version

putStrLnErr :: String -> IO ()
putStrLnErr m = hPutStrLn stderr ("*ERROR* " ++ m) >> exitFailure

putStrLnWarn :: String -> IO ()
putStrLnWarn m = hPutStrLn stderr ("*WARNING* " ++ m)

putStrLnInfo :: String -> IO ()
putStrLnInfo m = hPutStrLn stderr ("*INFO* " ++ m)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cabfn:xpkgs) -> do genTravisFromCabalFile cabfn xpkgs
        _ -> putStrLnErr (unlines $ [ "expected .cabal file as command-line argument"
                                    , "Usage: make_travis_yml_2.hs <cabal-file> <extra-apt-packages...>"
                                    , ""
                                    , "Example: make_travis_yml_2.hs someProject.cabal alex-3.1.7 liblzma-dev > .travis.yml"
                                    ])

genTravisFromCabalFile :: FilePath -> [String] -> IO ()
genTravisFromCabalFile fn xpkgs = do
    gpd <- readPackageDescription maxBound fn

    let compilers = testedWith $ packageDescription $ gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= GHC ]
        ghcVerConstrs = [ vc | (GHC,vc) <- compilers ]
        ghcVerConstrs' = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs

    when (null compilers) $ do
        putStrLnErr "empty or missing 'tested-with:' definition in .cabal file"

    unless (null unknownComps) $ do
        putStrLnWarn $ "ignoring unsupported compilers mentioned in tested-with: " ++ show unknownComps

    when (null ghcVerConstrs) $ do
        putStrLnErr "'tested-with:' doesn't mention any 'GHC' version"

    when (isNoVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' describes an empty version range for 'GHC'"

    when (isAnyVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' allows /any/ 'GHC' version"

    let testedGhcVersions = filter (`withinRange` ghcVerConstrs') knownGhcVersions

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    putStrLnInfo $ "Generating Travis-CI config for testing for GHC versions: " ++ (unwords $ map disp' $ testedGhcVersions)

    ----------------------------------------------------------------------------
    -- travis.yml generation starts here

    putStrLn "# This file has been generated by `make_travis_yml_2.hs`"
    putStrLn "# see https://github.com/hvr/multi-ghc-travis for more information"
    putStrLn "language: c"
    putStrLn "sudo: false"
    putStrLn ""
    putStrLn "cache:"
    putStrLn "  directories:"
    putStrLn "    - $HOME/.cabal/packages"
    putStrLn "    - $HOME/.cabal/store"
    putStrLn ""
    putStrLn "before_cache:"
    putStrLn "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
    putStrLn "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/00-index.tar"
    putStrLn ""
    putStrLn "matrix:"
    putStrLn "  include:"

    forM_ testedGhcVersions $ \gv -> do
        let cvs = disp' (lookupCabVer gv)
            gvs = disp' gv

            xpkgs' = concatMap (',':) xpkgs

        putStrLn $ concat [ "    - env: CABALVER=", cvs, " GHCVER=", gvs ]
        putStrLn $ concat [ "      compiler: \": #GHC ", gvs, "\"" ]
        putStrLn $ concat [ "      addons: {apt: {packages: [cabal-install-", cvs, ",ghc-", gvs, xpkgs'
                          , "], sources: [hvr-ghc]}}" ]
        return ()

    let headGhcVers = filter isHead testedGhcVersions

    unless (null headGhcVers) $ do
        putStrLn ""
        putStrLn "  allow_failures:"

    forM_ headGhcVers $ \gv -> do
        let cvs = disp' (lookupCabVer gv)
            gvs = disp' gv
        putStrLn $ concat [ "    - env: CABALVER=", cvs, " GHCVER=", gvs ]

    putStrLn ""
    putStrLn "before_install:"
    putStrLn " - unset CC"
    putStrLn " - export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH"

    putStrLn ""

    putStr $ unlines
        [ "install:"
        , " - cabal --version"
        , " - BENCH=${BENCH---enable-benchmarks}"
        , " - TEST=${TEST---enable-tests}"
        , " - echo \"$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , " - if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ];"
        , "   then"
        , "     zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz >"
        , "          $HOME/.cabal/packages/hackage.haskell.org/00-index.tar;"
        , "   fi"
        , " - travis_retry cabal update -v"
        , " - sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , " - cabal new-build ${TEST} ${BENCH} --dep"
        , ""
        , "# Here starts the actual work to be performed for the package under test;"
        , "# any command which exits with a non-zero exit code causes the build to fail."
        , "script:"
        , " - if [ -f configure.ac ]; then autoreconf -i; fi"
        , " # this builds all libraries and executables (including tests/benchmarks)"
        , " - cabal new-build ${TEST} ${BENCH} -v2  # -v2 provides useful information for debugging"
        , ""
        , " # there's no 'cabal new-test' yet, so let's emulate for now"
        , " - TESTS=( $(awk 'tolower($0) ~ /^test-suite / { print $2 }' *.cabal) );"
        , "   RC=true; for T in ${TESTS[@]}; do echo \"== $T ==\";"
        , "   if dist-newstyle/build/*/build/$T/$T; then echo \"= $T OK =\";"
        , "   else echo \"= $T FAILED =\"; RC=false; fi; done; $RC"
        , " - cabal sdist # test that a source-distribution can be generated"
        , ""
        , " # Check that the resulting source distribution can be built w/o and w tests"
        , " - SRC_BASENAME=$(cabal info . | awk '{print $2;exit}')"
        , " - tar -C dist/ -xf dist/$SRC_BASENAME.tar.gz"
        , " - \"echo 'packages: *.cabal' > dist/$SRC_BASENAME/cabal.project\""
        , " - cd dist/$SRC_BASENAME/"
        , " - cabal new-build --disable-tests --disable-benchmarks"
        , " - rm -rf ./dist-newstyle"
        , " - cabal new-build ${TEST} ${BENCH}"
        , ""
        , "# EOF"
        ]

    return ()
  where
    knownGhcVersions :: [Version]
    knownGhcVersions = fmap (`Version` [])
                       [ [7,0,1],  [7,0,2], [7,0,3], [7,0,4]
                       , [7,2,1],  [7,2,2]
                       , [7,4,1],  [7,4,2]
                       , [7,6,1],  [7,6,2], [7,6,3]
                       , [7,8,1],  [7,8,2], [7,8,3], [7,8,4]
                       , [7,10,1], [7,10,2], [7,10,3]
                       , [8,0,1]
                       , [8,1]  -- HEAD
                       ]

    lookupCabVer :: Version -> Version
    lookupCabVer (Version (x:y:_) _) = maybe (error "internal error") id $ lookup (x,y) cabalVerMap
      where
        cabalVerMap = fmap (fmap (`Version` []))
                      [ ((7, 0),  [1,24])
                      , ((7, 2),  [1,24])
                      , ((7, 4),  [1,24])
                      , ((7, 6),  [1,24])
                      , ((7, 8),  [1,24])
                      , ((7,10),  [1,24])
                      , ((8, 0),  [1,24])
                      , ((8, 1),  [1,25]) -- HEAD
                      ]

    isHead (Version (_:y:_) _) = odd (y :: Int)

    disp' v | isHead v = "head"
            | otherwise = display v
