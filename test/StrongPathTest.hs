{-# LANGUAGE QuasiQuotes #-}

module StrongPathTest where

import Data.Maybe (fromJust)
import StrongPath
import Test.Tasty.Hspec
import Test.Utils

data Bar

data Fizz

-- TODO: I should look into using QuickCheck to simplify / enhcance StrongPath tests,
--       it would probably be a good fit for some cases.

spec_StrongPath :: Spec
spec_StrongPath = do
  it "Example with Foo file and Bar, Fizz and Kokolo dirs" $ do
    let fooFileInBarDir = [relfile|foo.txt|] :: Path' (Rel Bar) File'
    let barDirInFizzDir = [reldir|kokolo/bar|] :: Path' (Rel Fizz) (Dir Bar)
    let fizzDir = systemSpRoot </> [reldir|fizz|] :: Path' Abs (Dir Fizz)
    let fooFile = (fizzDir </> barDirInFizzDir </> fooFileInBarDir) :: Path' Abs File'
    let fooFileInFizzDir = (barDirInFizzDir </> fooFileInBarDir) :: Path' (Rel Fizz) File'
    fromAbsFile fooFile `shouldBe` posixToSystemFp "/fizz/kokolo/bar/foo.txt"
    fromRelFile fooFileInFizzDir `shouldBe` posixToSystemFp "kokolo/bar/foo.txt"

  describe "`parent` correctly returns parent dir" $ do
    let test msp mexpectedSp =
          it ("parent (" ++ show msp ++ ") == " ++ show mexpectedSp) $ do
            let sp = fromJust msp
            let expectedSp = fromJust mexpectedSp
            parent sp `shouldBe` expectedSp
    let tests relDirParser relFileParser absDirParser absFileParser root = do
          test (relDirParser "a/b") (relDirParser "a")
          test (relDirParser "../a") (relDirParser "..")
          test (relDirParser "..") (relDirParser "../..")
          test (relDirParser ".") (relDirParser "..")
          test (relFileParser "a.txt") (relDirParser ".")
          test (relFileParser "../a.txt") (relDirParser "..")
          test (relFileParser "a/b.txt") (relDirParser "a")
          test (absDirParser $ root ++ "a/b") (absDirParser $ root ++ "a")
          test (absDirParser root) (absDirParser root)
          test (absFileParser $ root ++ "a/b.txt") (absDirParser $ root ++ "a")
    describe "when standard is System" $
      tests parseRelDir parseRelFile parseAbsDir parseAbsFile systemFpRoot
    describe "when standard is Windows" $
      tests parseRelDirW parseRelFileW parseAbsDirW parseAbsFileW "C:\\"
    describe "when standard is Posix" $
      tests parseRelDirP parseRelFileP parseAbsDirP parseAbsFileP "/"

  describe "</> correctly concatenates two corresponding paths" $ do
    let test mlsp mrsp mexpectedSp =
          it (show mlsp ++ " </> " ++ show mrsp ++ " == " ++ show mexpectedSp) $ do
            let lsp = fromJust mlsp
            let rsp = fromJust mrsp
            let expectedSp = fromJust mexpectedSp
            (lsp </> rsp) `shouldBe` expectedSp
    let tests relDirParser relFileParser absDirParser absFileParser root = do
          test (relDirParser "a/b") (relFileParser "c.txt") (relFileParser "a/b/c.txt")
          test (relDirParser "a/b") (relFileParser "../c.txt") (relFileParser "a/c.txt")
          test (relDirParser "..") (relFileParser "b/c.txt") (relFileParser "../b/c.txt")
          test (relDirParser "..") (relFileParser "../c.txt") (relFileParser "../../c.txt")
          test (relDirParser "..") (relDirParser "..") (relDirParser "../..")
          test (relDirParser ".") (relDirParser "../a") (relDirParser "../a")
          test (relDirParser ".") (relDirParser ".") (relDirParser ".")
          test (relDirParser "a/b") (relDirParser "c/d") (relDirParser "a/b/c/d")
          test (relDirParser "../a/b") (relDirParser "c/d") (relDirParser "../a/b/c/d")
          test (absDirParser $ root ++ "a/b") (relFileParser "c.txt") (absFileParser $ root ++ "a/b/c.txt")
          test (absDirParser $ root ++ "a/b") (relFileParser "../c.txt") (absFileParser $ root ++ "a/c.txt")
          test (absDirParser $ root ++ "a") (relDirParser "../b") (absDirParser $ root ++ "b")
          test (absDirParser $ root ++ "a/b") (relDirParser "../../../") (absDirParser root)
    describe "when standard is System" $
      tests parseRelDir parseRelFile parseAbsDir parseAbsFile systemFpRoot
    describe "when standard is Windows" $
      tests parseRelDirW parseRelFileW parseAbsDirW parseAbsFileW "C:\\"
    describe "when standard is Posix" $
      tests parseRelDirP parseRelFileP parseAbsDirP parseAbsFileP "/"

  describe "`basename` correctly returns filename/dirname" $ do
    let test msp mexpectedSp =
          it ("basename (" ++ show msp ++ ") == " ++ show mexpectedSp) $ do
            let sp = fromJust msp
            let expectedSp = fromJust mexpectedSp
            basename sp `shouldBe` expectedSp
    let tests relDirParser relFileParser absDirParser absFileParser root = do
          test (absFileParser $ root ++ "a/b/c.txt") (relFileParser "c.txt")
          test (absDirParser $ root ++ "a/b") (relDirParser "b")
          test (absDirParser root) (relDirParser ".")
          test (relFileParser "file.txt") (relFileParser "file.txt")
          test (relFileParser "../file.txt") (relFileParser "file.txt")
          test (relDirParser ".") (relDirParser ".")
          test (relDirParser "..") (relDirParser "..")
          test (relDirParser "../..") (relDirParser "..")
    describe "when standard is System" $
      tests parseRelDir parseRelFile parseAbsDir parseAbsFile systemFpRoot
    describe "when standard is Windows" $
      tests parseRelDirW parseRelFileW parseAbsDirW parseAbsFileW "C:\\"
    describe "when standard is Posix" $
      tests parseRelDirP parseRelFileP parseAbsDirP parseAbsFileP "/"

  describe "relDirToPosix/relFileToPosix correctly converts any relative path to relative posix path" $ do
    describe "when strong path is relative dir" $ do
      let expectedPosixPath = [reldirP|test/dir/|]
      it "from standard Win" $
        fromJust (relDirToPosix [reldirW|test\dir\|])
          `shouldBe` expectedPosixPath
      it "from standard Posix" $
        fromJust (relDirToPosix [reldirP|test/dir/|])
          `shouldBe` expectedPosixPath
      it "from standard System" $
        fromJust (relDirToPosix [reldir|test/dir/|])
          `shouldBe` expectedPosixPath
    describe "correctly when strong path is relative file" $ do
      let expectedPosixPath = [relfileP|test/file|]
      it "from standard Win" $
        fromJust (relFileToPosix [relfileW|test\file|])
          `shouldBe` expectedPosixPath
      it "from standard Posix" $
        fromJust (relFileToPosix [relfileP|test/file|])
          `shouldBe` expectedPosixPath
      it "from standard System" $
        fromJust (relFileToPosix [relfileP|test/file|])
          `shouldBe` expectedPosixPath

systemSpRoot :: Path' Abs Dir'
systemSpRoot = fromJust $ parseAbsDir systemFpRoot
