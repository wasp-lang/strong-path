module StrongPath.InstanceTest where

import Data.Hashable
import StrongPath
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

test_StrongPathInstance :: IO TestTree
test_StrongPathInstance = testSpec "StrongPath.Instance" $ do
  it "Hashable returns hash of underlying filepath" $ do
    let rawPath = "/abPath/dir/"
    path <- parseAbsDir rawPath
    hash rawPath `shouldBe` hash path