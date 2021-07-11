{-# LANGUAGE QuasiQuotes #-}

module StrongPath.PathTest where

import Data.Maybe (fromJust)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.Path
import qualified System.FilePath as FP
import Test.Hspec
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Utils

test_StrongPathPath :: IO TestTree
test_StrongPathPath = testSpec "StrongPath.Path" $ do
  it "Conversion from Path to StrongPath and back returns original value." $ do
    let test pack unpack path = unpack (pack path) == path `shouldBe` True
    test fromPathRelFile toPathRelFile [P.relfile|some/file.txt|]
    test fromPathRelDir toPathRelDir [P.reldir|some/dir/|]
    test fromPathAbsFile toPathAbsFile $ systemPathRoot P.</> [P.relfile|some/file.txt|]
    test fromPathAbsDir toPathAbsDir $ systemPathRoot P.</> [P.reldir|some/dir|]

    test fromPathRelFileP toPathRelFileP [PP.relfile|some/file.txt|]
    test fromPathRelDirP toPathRelDirP [PP.reldir|some/dir/|]
    test fromPathAbsFileP toPathAbsFileP [PP.absfile|/some/file.txt|]
    test fromPathAbsDirP toPathAbsDirP [PP.absdir|/some/dir|]

    test fromPathRelFileW toPathRelFileW [PW.relfile|some\file.txt|]
    test fromPathRelDirW toPathRelDirW [PW.reldir|some\dir\|]
    test fromPathAbsFileW toPathAbsFileW [PW.absfile|C:\some\file.txt|]
    test fromPathAbsDirW toPathAbsDirW [PW.absdir|C:\some\dir|]

systemPathRoot :: P.Path P.Abs P.Dir
systemPathRoot = fromJust $ P.parseAbsDir systemFpRoot
