{-# OPTIONS_HADDOCK hide #-}

module StrongPath.FilePath
  ( -- ** Parsers (from 'FilePath' to 'Path')
    -- $parsersFilepath
    parseRelDir,
    parseRelFile,
    parseAbsDir,
    parseAbsFile,
    parseRelDirW,
    parseRelFileW,
    parseAbsDirW,
    parseAbsFileW,
    parseRelDirP,
    parseRelFileP,
    parseAbsDirP,
    parseAbsFileP,

    -- ** Conversion (from 'Path' to 'FilePath')
    -- $conversionFilepath
    toFilePath,
    fromRelDir,
    fromRelFile,
    fromAbsDir,
    fromAbsFile,
    fromRelDirP,
    fromRelFileP,
    fromAbsDirP,
    fromAbsFileP,
    fromRelDirW,
    fromRelFileW,
    fromAbsDirW,
    fromAbsFileW,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.List (intercalate)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.Internal
import StrongPath.Path
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW

-- $parsersFilepath
-- Path can be constructed from `FilePath`:
--
-- > parse<base><type><standard> :: MonadThrow m => FilePath -> m (<corresponding_path_type>)
--
-- There are 12 parser functions, each of them parsing 'FilePath' into a specific 'Path'
-- type.
-- All of them work in the same fashion and will throw an error (via 'MonadThrow')
-- if given 'FilePath' can't be parsed into the specific 'Path' type.
-- For example, if path is absolute, 'parseRelDir' will throw an error.
--
-- Not all parsers accept all types of separators, for example
-- 'parseRelDirP' parser will fail to parse paths using Windows separators,
-- while 'parseRelDirW' will accept both Windows and Posix separators.
--
-- Below is a table describing, for all the parser functions,
-- which path standard (separators) do they accept as input
-- and to what path standard they parse it.
--
-- +---------------------------+-----------------+----------+
-- |          Parsers          |      From       |    To    |
-- +===========================+=================+==========+
-- | parse[Abs|Rel][Dir|File]  |  System/Posix   |  System  |
-- +---------------------------+-----------------+----------+
-- | parse[Abs|Rel][Dir|File]W |  Win/Posix      |   Win    |
-- +---------------------------+-----------------+----------+
-- | parse[Abs|Rel][Dir|File]P |   Posix         |  Posix   |
-- +---------------------------+-----------------+----------+
--
-- NOTE: Root of @parseAbs...@ input always has to match its path standard!
--   e.g., 'parseAbsDirW' can parse @\"C:\\foo\/bar\"@ but it can't parse @\"\/foo\/bar\"@.
--
-- Examples:
--
--  - @parseAbsFile \"C:\\foo\\bar.txt\"@ is valid if system is Windows, and gives the same result as @parseAbsFile \"C:\\foo\/bar.txt\"@.
--    On the other hand, both are invalid if system is Linux.
--  - @parseRelFile \"foo\/bar.txt\"@ is valid independent of the system.
--  - @parseRelFile \"foo\\bar.txt\"@ is valid only if system is Windows.
--  - @parseRelDirW \"foo\\bar\\test\"@ is valid, independent of the system, and gives the same result as @parseRelDirW \"foo\\bar\/test\"@ or @parseRelDirW "foo\/bar\/test\"@.
--
-- Basically, all of the parsers accept their \"native\" standard AND Posix,
-- which enables you to hardcode paths as Posix in the code that will compile
-- and work both on Linux and Windows when using `System` as a standard.
-- So Posix becames a kind of \"universal\" language for hardcoding the paths.

parseRelDir :: MonadThrow m => FilePath -> m (Path System (Rel d1) (Dir d2))
parseRelFile :: MonadThrow m => FilePath -> m (Path System (Rel d) (File f))
parseAbsDir :: MonadThrow m => FilePath -> m (Path System Abs (Dir d))
parseAbsFile :: MonadThrow m => FilePath -> m (Path System Abs (File f))
parseRelDirW :: MonadThrow m => FilePath -> m (Path Windows (Rel d1) (Dir d2))
parseRelFileW :: MonadThrow m => FilePath -> m (Path Windows (Rel d) (File f))
parseAbsDirW :: MonadThrow m => FilePath -> m (Path Windows Abs (Dir d))
parseAbsFileW :: MonadThrow m => FilePath -> m (Path Windows Abs (File f))
parseRelDirP :: MonadThrow m => FilePath -> m (Path Posix (Rel d1) (Dir d2))
parseRelFileP :: MonadThrow m => FilePath -> m (Path Posix (Rel d) (File f))
parseAbsDirP :: MonadThrow m => FilePath -> m (Path Posix Abs (Dir d))
parseAbsFileP :: MonadThrow m => FilePath -> m (Path Posix Abs (File f))
---- System
parseRelDir = parseRelDirFP RelDir [FP.pathSeparator, FPP.pathSeparator] P.parseRelDir

parseRelFile = parseRelFileFP RelFile [FP.pathSeparator, FPP.pathSeparator] P.parseRelFile

parseAbsDir fp = fromPathAbsDir <$> P.parseAbsDir fp

parseAbsFile fp = fromPathAbsFile <$> P.parseAbsFile fp

---- Windows
parseRelDirW = parseRelDirFP RelDirW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelDir

parseRelFileW = parseRelFileFP RelFileW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelFile

parseAbsDirW fp = fromPathAbsDirW <$> PW.parseAbsDir fp

parseAbsFileW fp = fromPathAbsFileW <$> PW.parseAbsFile fp

---- Posix
parseRelDirP = parseRelDirFP RelDirP [FPP.pathSeparator] PP.parseRelDir

parseRelFileP = parseRelFileFP RelFileP [FPP.pathSeparator] PP.parseRelFile

parseAbsDirP fp = fromPathAbsDirP <$> PP.parseAbsDir fp

parseAbsFileP fp = fromPathAbsFileP <$> PP.parseAbsFile fp

-- $conversionFilepath
-- 'Path' can be converted into 'FilePath' via polymorphic function 'toFilePath'
-- or via any of the 12 functions that accept specific path type.
--
-- We recommend using specific functions instead of 'toFilePath',
-- because that way you are explicit about which path you expect
-- and if that expectancy is not met, type system will catch it.

toFilePath :: Path s b t -> FilePath
toFilePath sp = case sp of
  ---- System
  RelDir p prefix -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
  RelFile p prefix -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
  AbsDir p -> P.toFilePath p
  AbsFile p -> P.toFilePath p
  ---- Windows
  RelDirW p prefix -> relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
  RelFileW p prefix -> relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
  AbsDirW p -> PW.toFilePath p
  AbsFileW p -> PW.toFilePath p
  ---- Posix
  RelDirP p prefix -> relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
  RelFileP p prefix -> relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
  AbsDirP p -> PP.toFilePath p
  AbsFileP p -> PP.toFilePath p
  where
    relPathToFilePath pathToFilePath sep prefix path =
      combinePrefixWithPath sep (relPathPrefixToFilePath sep prefix) (pathToFilePath path)

    relPathPrefixToFilePath :: Char -> RelPathPrefix -> FilePath
    relPathPrefixToFilePath _ NoPrefix = ""
    relPathPrefixToFilePath sep (ParentDir n) =
      intercalate [sep] (replicate n "..") ++ [sep]

    -- TODO: This function and helper functions above are somewhat too loose and hard to
    --   follow, implement them in better way.
    -- Here we are assuming that prefix is of form (../)*, therefore it ends with separator,
    -- and it could also be empty.
    combinePrefixWithPath :: Char -> String -> FilePath -> FilePath
    combinePrefixWithPath sep prefix path
      | path `elem` [".", ['.', sep], "./"] && not (null prefix) = prefix
    combinePrefixWithPath _ prefix path = prefix ++ path

-- These functions just call toFilePath, but their value is in
-- their type: they allow you to capture expected type of the strong path
-- that you want to convert into FilePath.
fromRelDir :: Path System (Rel r) (Dir d) -> FilePath
fromRelDir = toFilePath

fromRelFile :: Path System (Rel r) (File f) -> FilePath
fromRelFile = toFilePath

fromAbsDir :: Path System Abs (Dir d) -> FilePath
fromAbsDir = toFilePath

fromAbsFile :: Path System Abs (File f) -> FilePath
fromAbsFile = toFilePath

fromRelDirP :: Path Posix (Rel r) (Dir d) -> FilePath
fromRelDirP = toFilePath

fromRelFileP :: Path Posix (Rel r) (File f) -> FilePath
fromRelFileP = toFilePath

fromAbsDirP :: Path Posix Abs (Dir d) -> FilePath
fromAbsDirP = toFilePath

fromAbsFileP :: Path Posix Abs (File f) -> FilePath
fromAbsFileP = toFilePath

fromRelDirW :: Path Windows (Rel r) (Dir d) -> FilePath
fromRelDirW = toFilePath

fromRelFileW :: Path Windows (Rel r) (File f) -> FilePath
fromRelFileW = toFilePath

fromAbsDirW :: Path Windows Abs (Dir d) -> FilePath
fromAbsDirW = toFilePath

fromAbsFileW :: Path Windows Abs (File f) -> FilePath
fromAbsFileW = toFilePath
