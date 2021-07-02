{-# LANGUAGE QuasiQuotes #-}

module PathTest where

import Data.Maybe (fromJust)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import qualified System.FilePath as FP
import Test.Tasty.Hspec

spec_Path :: Spec
spec_Path = do
  -- Just checking that Path behaves in a way that we expect it to behave.
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

  -- NOTE: All of the failing Path tests are due to the badly implemented Include mechanism in Path.
  --   I made a PR for fix on Path, so when that gets in we can uncomment these tests and also remove
  --   workarounds in StrongPath / StrongPath.Internal.

  -- describe "Concatenation of Win . paths works as expected" $ do
  --     let test lp rp ep =
  --             it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
  --                 (lp PW.</> rp) `shouldBe` ep
  --     -- TODO: Fails on Linux/Mac: expected: ".\\" but got: ".\\.\\"
  --     test [PW.reldir|.|] [PW.reldir|.|] [PW.reldir|.|]
  --     -- TODO: Fails on Linux/Mac: expected: "a\\" but got: ".\\a\\"
  --     test [PW.reldir|.|] [PW.reldir|a|] [PW.reldir|a|]
  --     -- TODO: Fails on Linux/Mac: expected: "a\\" but got: "a\\.\\"
  --     test [PW.reldir|a|] [PW.reldir|.|] [PW.reldir|a|]

  -- describe "Concatenation of Posix . paths works as expected" $ do
  --     let test lp rp ep =
  --             it (show lp ++ " </> " ++ show rp ++ " == " ++ show ep) $
  --                 (lp PP.</> rp) `shouldBe` ep
  --     -- TODO: Fails on Win: expected: "./" but got: "././"
  --     test [PP.reldir|.|] [PP.reldir|.|] [PP.reldir|.|]
  --     -- TODO: Fails on Win: expected: "a/" but got: "./a/"
  --     test [PP.reldir|.|] [PP.reldir|a|] [PP.reldir|a|]
  --     -- TODO: Fails on Win: expected: "a/" but got: "a/./"
  --     test [PP.reldir|a|] [PP.reldir|.|] [PP.reldir|a|]

  describe "Parsing rel path with .. at start should fail" $ do
    let test parser p =
          it (show p ++ " should successfully parse") $
            parser p `shouldBe` Nothing
    describe "for PW.parseRelDir" $ do
      test PW.parseRelDir "../a"
    -- -- TODO: This fails on Linux/Mac! Weird, I thought Path does not allow relative paths starting with ..?
    -- --       expected: Nothing but got: Just "..\\a\\"
    -- test PW.parseRelDir "..\\a"
    describe "for P.parseRelDir" $ do
      test P.parseRelDir "../a"
      test P.parseRelDir $ ".." FP.</> "a"
    describe "for PP.parseRelDir" $ do
      test PP.parseRelDir "../a"
