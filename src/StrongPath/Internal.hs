{-# LANGUAGE DeriveLift #-}

module StrongPath.Internal where

import Control.Monad.Catch (MonadThrow)
import Data.Data (Typeable)
import Language.Haskell.TH.Syntax (Lift)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW

-- | Strongly typed file path. Central type of the "StrongPath".
--
--   [@s@]: __Standard__: Posix or windows. Can be fixed ('Posix', 'Windows') or determined by the system ('System').
--
--   [@b@]: __Base__: Absolute ('Abs') or relative ('Rel').
--
--   [@t@]: __Type__: File ('File') or directory ('Dir').
--
-- Some examples:
--
-- > Path System (Dir HomeDir) (File FooFile)
-- > Path System Abs (Dir HomeDir)
-- > Path Posix (Rel ProjectRoot) (File ())
data Path s b t
  = -- System
    RelDir (P.Path P.Rel P.Dir) RelPathPrefix
  | RelFile (P.Path P.Rel P.File) RelPathPrefix
  | AbsDir (P.Path P.Abs P.Dir)
  | AbsFile (P.Path P.Abs P.File)
  | -- Windows
    RelDirW (PW.Path PW.Rel PW.Dir) RelPathPrefix
  | RelFileW (PW.Path PW.Rel PW.File) RelPathPrefix
  | AbsDirW (PW.Path PW.Abs PW.Dir)
  | AbsFileW (PW.Path PW.Abs PW.File)
  | -- Posix
    RelDirP (PP.Path PP.Rel PP.Dir) RelPathPrefix
  | RelFileP (PP.Path PP.Rel PP.File) RelPathPrefix
  | AbsDirP (PP.Path PP.Abs PP.Dir)
  | AbsFileP (PP.Path PP.Abs PP.File)
  deriving (Show, Eq, Lift, Typeable)

data RelPathPrefix
  = -- | ../, Int saying how many times it repeats.
    ParentDir Int
  | NoPrefix
  deriving (Show, Eq, Lift, Typeable)

-- | Describes 'Path' base as absolute.
data Abs deriving (Lift, Typeable)

-- | Describes 'Path' base as relative to the directory @dir@.
data Rel dir deriving (Lift, Typeable)

-- | Means that path points to a directory @dir@.
-- To use as a type in place of @dir@, we recommend creating an empty
-- data type representing the specific directory, e.g. @data ProjectRootDir@.
data Dir dir deriving (Lift, Typeable)

-- | Means that path points to a file @file@.
-- To use as a type in place of @file@, we recommend creating an empty
-- data type representing the specific file, e.g. @data ProjectManifestFile@.
data File file deriving (Lift, Typeable)

-- | Describes 'Path' standard as posix (e.g. @\/path\/to\/foobar@).
-- This makes 'Path' behave in system-independent fashion: code behaves the same
-- regardless of the system it is running on.
-- You will normally want to use it when dealing with paths from some external source,
-- or with paths that have explicitely fixed standard.
-- For example, if you are running your Haskell program on Windows and parsing logs which
-- were obtained from the Linux server, or maybe you are parsing Javascript import statements,
-- you will want to use 'Posix'.
data Posix deriving (Lift, Typeable)

-- | Describes 'Path' standard as windows (e.g. @C:\\path\\to\\foobar@).
-- Check 'Posix' for more details, everything is analogous.
data Windows deriving (Lift, Typeable)

-- | Describes 'Path' standard to be determined by the system/OS.
--
-- If the system is Windows, it will resolve to 'Windows' internally, and if not,
-- it will resolve to 'Posix'.
--
-- However, keep in mind that even if running on Windows, @Path Windows b t@
-- and @Path System b t@ are still considered to be different types,
-- even though @Path System b t @ internally uses Windows standard.
--
-- You will normally want to use 'System' if you are dealing with the paths on the disk of the host OS
-- (where your code is running), for example if user is providing you with the path to the file on the disk
-- that you will be doing something with.
-- Keep in mind that 'System' causes the behaviour of 'Path' to be system/platform-dependant.
data System deriving (Lift, Typeable) -- Depends on the platform, it is either Posix or Windows.

-- | 'System' is the most commonly used standard, so we provide you with a type alias for it.
type Path' = Path System

-- | When you don't want your path to be relative to anything specific,
-- it is convenient to use unit @()@.
type Rel' = Rel ()

-- | When you don't want your directory path to be named,
-- it is convenient to use unit @()@.
type Dir' = Dir ()

-- | When you don't want your file path to be named,
-- it is convenient to use unit @()@.
type File' = File ()

parseRelFP ::
  MonadThrow m =>
  (P.Path pb pt -> RelPathPrefix -> Path s (Rel d) t) ->
  [Char] ->
  (FilePath -> m (P.Path pb pt)) ->
  FilePath ->
  m (Path s (Rel d) t)
parseRelFP constructor validSeparators pathParser fp =
  let (prefix, fp') = extractRelPathPrefix validSeparators fp
      fp'' = if fp' == "" then "." else fp' -- Because Path Rel parsers can't handle just "".
   in (\p -> constructor p prefix) <$> pathParser fp''

-- | Extracts a multiple "../" from start of the file path.
--   If path is completely ../../.., also handles the last one.
--   NOTE: We don't normalize path in any way.
extractRelPathPrefix :: [Char] -> FilePath -> (RelPathPrefix, FilePath)
extractRelPathPrefix validSeparators path =
  let (n, path') = dropParentDirs path
   in (if n == 0 then NoPrefix else ParentDir n, path')
  where
    parentDirStrings :: [String]
    parentDirStrings = [['.', '.', s] | s <- validSeparators]

    pathStartsWithParentDir :: FilePath -> Bool
    pathStartsWithParentDir p = take 3 p `elem` parentDirStrings

    dropParentDirs :: FilePath -> (Int, FilePath)
    dropParentDirs p
      | pathStartsWithParentDir p =
        let (n, p') = dropParentDirs (drop 3 p)
         in (1 + n, p')
      | p == ".." = (1, "")
      | otherwise = (0, p)

-- NOTE: These three funtions, pathWinCombine... exist only to fix
--   Path.Windows.</> behaviour regarding concatenating '.' rel dirs
--   with other paths. While for Path.System and Path.Posix this concatenation
--   behaves as expected on Linux, Path.Windows behaves differently!
--   In more details:
--   [P.reldir|.|] P.</> [P.reldir|a|] results in [P.reldir|a|]
--   however
--   [PW.reldir|.|] PW.</> [PW.reldir|a|] results in [PW.reldir|.\\a|]
--   To summarize it, for System/Posix, Path behaves as:
--   . </> a = a
--   . </> . = .
--   a </> a = a
--   While for Windows, Path behaves as:
--   . </> a = .\a
--   . </> . = .\.
--   a </> . = a\.
--   which we don't want, we want it to behave same as for System/Posix.
--   That is why we handle these cases as special cases and then we let the Path.Windows.</>
--   do the rest of the work.
pathWinCombineRelDirAndRelFile :: PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.File -> PW.Path PW.Rel PW.File
pathWinCombineRelDirAndRelFile lp rp
  | PW.toFilePath lp == ['.', FPW.pathSeparator] = rp
  | otherwise = lp PW.</> rp

pathWinCombineRelDirAndRelDir :: PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.Dir
pathWinCombineRelDirAndRelDir lp rp
  | PW.toFilePath lp == ['.', FPW.pathSeparator] = rp
  | PW.toFilePath rp == ['.', FPW.pathSeparator] = lp
  | otherwise = lp PW.</> rp

pathWinCombineAbsDirAndRelDir :: PW.Path PW.Abs PW.Dir -> PW.Path PW.Rel PW.Dir -> PW.Path PW.Abs PW.Dir
pathWinCombineAbsDirAndRelDir lp rp
  | PW.toFilePath rp == ['.', FPW.pathSeparator] = lp
  | otherwise = lp PW.</> rp

-- NOTE: Same as pathWinCombineRelDirAndRelFile but for Posix (Path has the same problem).
pathPosixCombineRelDirAndRelFile :: PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.File -> PP.Path PP.Rel PP.File
pathPosixCombineRelDirAndRelFile lp rp
  | PP.toFilePath lp == ['.', FPP.pathSeparator] = rp
  | otherwise = lp PP.</> rp

pathPosixCombineRelDirAndRelDir :: PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.Dir
pathPosixCombineRelDirAndRelDir lp rp
  | PP.toFilePath lp == ['.', FPP.pathSeparator] = rp
  | PP.toFilePath rp == ['.', FPP.pathSeparator] = lp
  | otherwise = lp PP.</> rp

pathPosixCombineAbsDirAndRelDir :: PP.Path PP.Abs PP.Dir -> PP.Path PP.Rel PP.Dir -> PP.Path PP.Abs PP.Dir
pathPosixCombineAbsDirAndRelDir lp rp
  | PP.toFilePath rp == ['.', FPP.pathSeparator] = lp
  | otherwise = lp PP.</> rp

prefixNumParentDirs :: RelPathPrefix -> Int
prefixNumParentDirs NoPrefix = 0
prefixNumParentDirs (ParentDir n) = n

relPathNumParentDirs :: Path s (Rel r) t -> Int
relPathNumParentDirs = prefixNumParentDirs . relPathPrefix

relPathPrefix :: Path s (Rel r) t -> RelPathPrefix
relPathPrefix sp = case sp of
  RelDir _ pr -> pr
  RelFile _ pr -> pr
  RelDirW _ pr -> pr
  RelFileW _ pr -> pr
  RelDirP _ pr -> pr
  RelFileP _ pr -> pr
  _ -> impossible

impossible :: a
impossible = error "This should be impossible."
