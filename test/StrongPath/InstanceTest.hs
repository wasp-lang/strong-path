module StrongPath.InstanceTest where

import Data.Hashable
import Data.List (sort)
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
  it "Paths can be compared" $ do
    aPath <- parseRelDir "a"
    bPath <- parseRelDir "b"
    aPath < bPath `shouldBe` True
    aPath > bPath `shouldBe` False
  it "Path can be sorted because they can be compared" $ do
    aPath <- parseRelDir "a"
    bPath <- parseRelDir "b"
    abPath <- parseRelDir "a/b"
    sort [bPath, abPath, aPath] `shouldBe` [aPath, abPath, bPath]