{-# LANGUAGE QuasiQuotes #-}

module PathTest where

import Data.Maybe (fromJust)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import qualified System.FilePath as FP
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

test_Path :: IO TestTree
test_Path = testSpec "Path" $ do
  -- Just checking that Path behaves in a way that we expect it to behave.
  -- At earlier versions of Path (< 0.9.0) there were bugs which made some of the tests below fail.
  -- This way we ensure those bugs are fixed and don't return.

  it "Path.Windows.parseRelDir correctly parses Windows path" $ do
    fromJust (PW.parseRelDir ".\\") `shouldBe` fromJust (PW.parseRelDir "./")
    fromJust (PW.parseRelDir "a\\\\b\\") `shouldBe` fromJust (PW.parseRelDir "a/b/")
    fromJust (PW.parseRelDir "a\\b") `shouldBe` fromJust (PW.parseRelDir "a/b")
    PW.toFilePath (fromJust $ PW.parseRelDir "a\\b\\") `shouldBe` "a\\b\\"

  describe "Concatenation of System . paths works as expected" $ do
    let test lp rp ep =
          it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
            (lp P.</> rp) `shouldBe` ep
    test [P.reldir|.|] [P.reldir|.|] [P.reldir|.|]
    test [P.reldir|a|] [P.reldir|.|] [P.reldir|a|]
    test [P.reldir|.|] [P.reldir|a|] [P.reldir|a|]
    test [P.reldir|.|] [P.relfile|c.txt|] [P.relfile|c.txt|]

  describe "Concatenation of Win . paths works as expected" $ do
    let test lp rp ep =
          it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
            (lp PW.</> rp) `shouldBe` ep
    test [PW.reldir|.|] [PW.reldir|.|] [PW.reldir|.|]
    test [PW.reldir|.|] [PW.reldir|a|] [PW.reldir|a|]
    test [PW.reldir|a|] [PW.reldir|.|] [PW.reldir|a|]

  describe "Concatenation of Posix . paths works as expected" $ do
    let test lp rp ep =
          it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
            (lp PP.</> rp) `shouldBe` ep
    test [PP.reldir|.|] [PP.reldir|.|] [PP.reldir|.|]
    test [PP.reldir|.|] [PP.reldir|a|] [PP.reldir|a|]
    test [PP.reldir|a|] [PP.reldir|.|] [PP.reldir|a|]

  describe "Parsing rel path with .. at start should fail" $ do
    let test parser p =
          it (show p ++ " should unsuccessfully parse") $
            parser p `shouldBe` Nothing
    describe "for PW.parseRelDir" $ do
      test PW.parseRelDir "../a"
      test PW.parseRelDir "..\\a"
    describe "for P.parseRelDir" $ do
      test P.parseRelDir "../a"
      test P.parseRelDir $ ".." FP.</> "a"
    describe "for PP.parseRelDir" $ do
      test PP.parseRelDir "../a"
