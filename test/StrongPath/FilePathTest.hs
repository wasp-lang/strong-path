module StrongPath.FilePathTest where

import Data.Maybe (fromJust)
import StrongPath.FilePath
import StrongPath.Internal
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Utils

test_StrongPathFilePath :: IO TestTree
test_StrongPathFilePath = testSpec "StrongPath.FilePath" $ do
  describe "Parsing FilePath into StrongPath" $ do
    let runTest fpToParseIntoExpectedFp parser fpToParse =
          let expectedFp = fpToParseIntoExpectedFp fpToParse
           in it (fpToParse ++ " should parse into " ++ expectedFp) $ do
                let sp = fromJust $ parser fpToParse
                toFilePath sp `shouldBe` expectedFp
    let runTestRel fpToParseIntoExpectedFp parser fpToParse expectedNumParentDirs =
          let expectedFp = fpToParseIntoExpectedFp fpToParse
           in it (fpToParse ++ " should parse into " ++ expectedFp) $ do
                let sp = fromJust $ parser fpToParse
                toFilePath sp `shouldBe` expectedFp
                relPathNumParentDirs sp `shouldBe` expectedNumParentDirs

    describe "into standard System" $ do
      describe "into base Rel" $ do
        describe "captures one or multiple ../ at start of relative path" $ do
          let test = runTestRel id
          test parseRelDir (posixToSystemFp "../../a/b/") 2
          test parseRelDir (posixToSystemFp "../") 1
          test parseRelDir (posixToSystemFp "../../") 2
          test parseRelDir (posixToSystemFp "./") 0
          test parseRelFile (posixToSystemFp "../a/b.txt") 1
        describe "can parse from system FilePath" $ do
          let test = runTestRel id
          test parseRelDir (posixToSystemFp "../a/b/") 1
          test parseRelDir (posixToSystemFp "a/b/") 0
          test parseRelFile (posixToSystemFp "../a/b.txt") 1
          test parseRelFile (posixToSystemFp "a/b.txt") 0
        describe "can parse from posix FilePath" $ do
          let test = runTestRel posixToSystemFp
          test parseRelDir "../a/b/" 1
          test parseRelDir "a/b/" 0
          test parseRelFile "../a/b.txt" 1
          test parseRelFile "a/b.txt" 0
      describe "into base Abs" $ do
        describe "can parse from system FilePath" $ do
          let test = runTest id
          test parseAbsDir (systemFpRoot FP.</> posixToSystemFp "a/b/")
          test parseAbsFile (systemFpRoot FP.</> posixToSystemFp "a/b.txt")
        describe "can parse from FilePath with system root and posix separators" $ do
          let test = runTest posixToSystemFp
          test parseAbsDir (systemFpRoot FP.</> "a/b/")
          test parseAbsFile (systemFpRoot FP.</> "a/b.txt")

    describe "into standard Windows" $ do
      describe "into base Rel" $ do
        describe "captures one or multiple ../ at start of relative path" $ do
          let test = runTestRel posixToWindowsFp
          test parseRelDirW (posixToSystemFp "../../a/b/") 2
          test parseRelFileW (posixToSystemFp "../a/b.txt") 1
        describe "can parse from windows FilePath" $ do
          let test = runTestRel id
          test parseRelDirW "..\\a\\b\\" 1
          test parseRelDirW "a\\b\\" 0
          test parseRelFileW "..\\a\\b.txt" 1
          test parseRelFileW "..\\..\\a\\b.txt" 2
          test parseRelFileW "a\\b.txt" 0
        describe "can parse from posix FilePath" $ do
          let test = runTestRel posixToWindowsFp
          test parseRelDirW "../a/b/" 1
          test parseRelDirW "a/b/" 0
          test parseRelFileW "../a/b.txt" 1
          test parseRelFileW "a/b.txt" 0
      describe "into base Abs" $ do
        describe "can parse from windows FilePath" $ do
          let test = runTest id
          test parseAbsDirW "C:\\a\\b\\"
          test parseAbsFileW "C:\\a\\b.txt"
        describe "can parse from FilePath with windows root and Posix separators" $ do
          let test = runTest posixToWindowsFp
          test parseAbsDirW "C:\\a/b/"
          test parseAbsFileW "C:\\a/b.txt"

    describe "into standard Posix" $ do
      describe "into base Rel" $ do
        describe "captures one or multiple ../ at start of relative path" $ do
          let test = runTestRel id
          test parseRelDirP "../../a/b/" 2
          test parseRelFileP "../a/b.txt" 1
        describe "can parse from posix FilePath" $ do
          let test = runTestRel id
          test parseRelDirP "../a/b/" 1
          test parseRelDirP "a/b/" 0
          test parseRelFileP "a/b.txt" 0
      describe "into base Abs" $ do
        describe "can parse from posix FilePath" $ do
          let test = runTest id
          test parseAbsDirP "/a/b/"
          test parseAbsFileP "/a/b.txt"

  describe "toFilePath correctly transforms StrongPath into FilePath" $ do
    let test msp efp =
          it ("toFilePath (" ++ show msp ++ ") = " ++ efp) $
            toFilePath (fromJust msp) `shouldBe` efp
    -- TODO: Add more tests.
    test (parseRelDir $ posixToSystemFp "../") (posixToSystemFp "../")
    test (parseRelDir $ posixToSystemFp "a/b") (posixToSystemFp "a/b/")
    test (parseRelFile $ posixToSystemFp "../../foo.txt") (posixToSystemFp "../../foo.txt")
    test (parseRelDirW "../") "..\\"
    test (parseRelDirP "../") "../"

  it "Parsing empty paths should fail" $ do
    let test parser p = parser p `shouldBe` Nothing
    test parseRelDir ""
    test parseRelFile ""
    test parseAbsDir ""
    test parseAbsFile ""
    test parseRelDirP ""
    test parseRelFileP ""
    test parseAbsDirP ""
    test parseAbsFileP ""
    test parseRelDirW ""
    test parseRelFileW ""
    test parseAbsDirW ""
    test parseAbsFileW ""

systemSpRoot :: Path' Abs Dir'
systemSpRoot = fromJust $ parseAbsDir systemFpRoot
