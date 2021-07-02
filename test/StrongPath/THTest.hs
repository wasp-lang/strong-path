{-# LANGUAGE QuasiQuotes #-}

module StrongPath.THTest where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import StrongPath.TH
import Test.Tasty.Hspec

spec_StrongPathTH :: Spec
spec_StrongPathTH = do
  describe "Quasi quoters generate expected values with expected types" $ do
    it "System" $ do
      [reldir|foo/bar/|] `shouldBe` fromJust (SP.parseRelDir "foo/bar/")
      [relfile|../foo/bar|] `shouldBe` fromJust (SP.parseRelFile "../foo/bar")
    -- NOTE: I don't test absdir and absfile here because I can't get that piece of code
    -- compile on both Win and Linux.

    it "Posix" $ do
      [reldirP|foo/bar/|] `shouldBe` fromJust (SP.parseRelDirP "foo/bar/")
      [relfileP|../foo/bar|] `shouldBe` fromJust (SP.parseRelFileP "../foo/bar")
      [absdirP|/foo/bar/|] `shouldBe` fromJust (SP.parseAbsDirP "/foo/bar/")
      [absfileP|/foo/bar|] `shouldBe` fromJust (SP.parseAbsFileP "/foo/bar")

    it "Windows" $ do
      [reldirW|foo/bar/|] `shouldBe` fromJust (SP.parseRelDirW "foo/bar/")
      [relfileW|..\foo/bar|] `shouldBe` fromJust (SP.parseRelFileW "..\\foo/bar")
      [absdirW|C:\foo\bar\|] `shouldBe` fromJust (SP.parseAbsDirW "C:\\foo\\bar\\")
      [absfileW|C:\foo\bar|] `shouldBe` fromJust (SP.parseAbsFileW "C:\\foo\\bar")
