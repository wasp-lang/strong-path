{-# LANGUAGE DeriveLift #-}

module StrongPath.Internal
  ( Path (..),
    RelPathPrefix (..),
    Abs,
    Rel,
    Dir,
    File,
    Posix,
    Windows,
    System,
    Path',
    File',
    Dir',
    Rel',
    parseRelFileFP,
    parseRelDirFP,
    impossible,
    prefixNumParentDirs,
    relPathNumParentDirs,
    relPathPrefix,
    extractRelPathPrefix,
  )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Language.Haskell.TH.Syntax (Lift)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW

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
  = -- NOTE: Relative paths can be sometimes be tricky when being reasoned about in the internal library code,
    --   when reconstructing them and working with them, due to RelPathPrefix and edge cases like ".", "..".
    --
    --   For example if original relative path was "..", we will parse it into RelDir "." ParentDir 1.
    --   Then it is important to be aware that this should be regarded as "..", and not "../.".
    --   In some functions like `basename` it is important to be aware of this.
    --
    --   Also, Path.Path can't hold empty path, so we can count on paths not to be empty.
    --
    --   And Path.Path can't store "." as file, only as dir, so that is also good to know.
    --
    --   I wonder if we could find a better way to represent path internaly, a way which would encode
    --   tricky situations explicitly, or maybe some kind of lower-level interface around it that would encode
    --   things like "paths can't be empty", "dir can be '.' but file can't", and similar.
    --   But maybe the solution would just be too complicated.
    -- System
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
  deriving (Show, Eq, Lift)

data RelPathPrefix
  = -- | ../, Int saying how many times it repeats.
    ParentDir Int
  | NoPrefix
  deriving (Show, Eq, Lift)

-- | Describes 'Path' base as absolute.
data Abs deriving (Lift)

-- | Describes 'Path' base as relative to the directory @dir@.
data Rel dir deriving (Lift)

-- | Means that path points to a directory @dir@.
-- To use as a type in place of @dir@, we recommend creating an empty
-- data type representing the specific directory, e.g. @data ProjectRootDir@.
data Dir dir deriving (Lift)

-- | Means that path points to a file @file@.
-- To use as a type in place of @file@, we recommend creating an empty
-- data type representing the specific file, e.g. @data ProjectManifestFile@.
data File file deriving (Lift)

-- | Describes 'Path' standard as posix (e.g. @\/path\/to\/foobar@).
-- This makes 'Path' behave in system-independent fashion: code behaves the same
-- regardless of the system it is running on.
-- You will normally want to use it when dealing with paths from some external source,
-- or with paths that have explicitely fixed standard.
-- For example, if you are running your Haskell program on Windows and parsing logs which
-- were obtained from the Linux server, or maybe you are parsing Javascript import statements,
-- you will want to use 'Posix'.
data Posix deriving (Lift)

-- | Describes 'Path' standard as windows (e.g. @C:\\path\\to\\foobar@).
-- Check 'Posix' for more details, everything is analogous.
data Windows deriving (Lift)

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
data System deriving (Lift) -- Depends on the platform, it is either Posix or Windows.

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

-- TODO: Extract `parseRelFileFP`, `parseRelDirFP`, `parseRelFP` and `extractRelPathPrefix` into StrongPath.FilePath.Internals?

parseRelFileFP ::
  MonadThrow m =>
  (p -> RelPathPrefix -> Path s (Rel d) (File f)) ->
  [Char] ->
  (FilePath -> m p) ->
  FilePath ->
  m (Path s (Rel d) (File f))
parseRelFileFP _ _ _ "" = throwM (P.InvalidRelFile "")
parseRelFileFP constructor validSeparators pathParser fp = parseRelFP constructor validSeparators pathParser fp

parseRelDirFP ::
  MonadThrow m =>
  (p -> RelPathPrefix -> Path s (Rel d1) (Dir d2)) ->
  [Char] ->
  (FilePath -> m p) ->
  FilePath ->
  m (Path s (Rel d1) (Dir d2))
parseRelDirFP _ _ _ "" = throwM (P.InvalidRelDir "")
parseRelDirFP constructor validSeparators pathParser fp = parseRelFP constructor validSeparators pathParser fp

-- Helper function for the parseRelFileFP and parseRelDirFP, should not be used called directly but only
-- by parseRelFileFP and parseRelDirFP.
parseRelFP ::
  MonadThrow m =>
  (p -> RelPathPrefix -> Path s (Rel d1) t) ->
  [Char] ->
  (FilePath -> m p) ->
  FilePath ->
  m (Path s (Rel d1) t)
parseRelFP _ _ _ "" = error "can't parse empty path"
parseRelFP constructor validSeparators pathParser fp = do
  let (prefix, fp') = extractRelPathPrefix validSeparators fp
      fp'' = if fp' == "" then "." else fp' -- Because Path Rel parsers can't handle just "".
  (\p -> constructor p prefix) <$> pathParser fp''

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
