module StrongPath.InstanceTest where

import Data.Hashable
import StrongPath
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

test_StrongPathInstance :: IO TestTree
test_StrongPathInstance = testSpec "StrongPath.Instance" $ do
  it "Different paths have different hash" $ do
    aPath <- parseRelDir "a"
    bPath <- parseRelDir "b"
    hash aPath `shouldNotBe` hash bPath
  it "Concatenated Paths have same hash as Path directly constructed from parts" $ do
    aPath <- parseRelDir "a"
    bPath <- parseRelDir "b"
    abPath <- parseRelDir "a/b"
    hash (aPath </> bPath) `shouldBe` hash abPath