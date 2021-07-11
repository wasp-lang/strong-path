module StrongPath.InternalTest where

import StrongPath.Internal
  ( RelPathPrefix (..),
    extractRelPathPrefix,
  )
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

test_StrongPathInternal :: IO TestTree
test_StrongPathInternal = testSpec "StrongPath.Internal" $ do
  describe "extractRelPathPrefix correctly extracts prefix from rel FilePath." $ do
    it "when path starts with multiple ../" $ do
      extractRelPathPrefix [FPP.pathSeparator] "../../" `shouldBe` (ParentDir 2, "")
      extractRelPathPrefix [FPP.pathSeparator] "../.." `shouldBe` (ParentDir 2, "")
      extractRelPathPrefix [FP.pathSeparator] ".." `shouldBe` (ParentDir 1, "")
      extractRelPathPrefix [FP.pathSeparator, FPP.pathSeparator] "../../../a/b" `shouldBe` (ParentDir 3, "a/b")
      extractRelPathPrefix [FPW.pathSeparator] "..\\a\\b" `shouldBe` (ParentDir 1, "a\\b")
    it "when path does not start with ../" $ do
      extractRelPathPrefix [FPP.pathSeparator] "a/b" `shouldBe` (NoPrefix, "a/b")
      extractRelPathPrefix [FP.pathSeparator] "b" `shouldBe` (NoPrefix, "b")
      extractRelPathPrefix [FP.pathSeparator] "." `shouldBe` (NoPrefix, ".")
