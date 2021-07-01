module StrongPath
  ( module StrongPath.Readme,

    -- * API

    -- ** Types

    -- *** Path
    Path,

    -- **** 'Path' type
    Dir,
    File,

    -- **** 'Path' base
    Abs,
    Rel,

    -- **** 'Path' standard
    -- $pathStandard
    Posix,
    Windows,
    System,

    -- **** 'Path' aliases
    Path',
    Rel',
    Dir',
    File',
    --
    module StrongPath.FilePath,

    -- ** Operations
    (</>),
    parent,

    -- ** Casting
    castRel,
    castDir,
    castFile,

    -- ** Conversion of path standard
    relDirToPosix,
    relFileToPosix,
    --
    module StrongPath.TH,

    -- ** Working with "Path" library

    -- | If you are using "Path" library alongside "StrongPath", you can import module "StrongPath.Path",
    -- which contains functions for converting "StrongPath" 'Path' into 'Path.Path' and vice versa.
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.FilePath
import StrongPath.Internal
import StrongPath.Readme
import StrongPath.TH
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW

-- TODO: I am getting warnings due to StrongPath.Readme being an empty module. Is there a way to avoid those?

-- TODO: Add relDirToWindows and relFileToWindows?
-- TODO: Implement relFile?

-- TODO: Can I use type classes and return type polymorhipsm to make all this shorter and reduce duplication?
-- class Path, and then I have PathWindows and PathPosix and PathSystem implement it, smth like that?
-- And then fromPathRelDir has polymorhic return type based on standard? I tried a little bit but it is complicated.

-- TODO: If there is no other solution to all this duplication, do some template haskell magic to simplify it.

-- $pathStandard
-- TLDR: If you are not sure which standard to use, go with 'System' since that is the most
-- common use case, and you will likely recognize the situation in which you need
-- system-indepenent behaviour ('Posix', 'Windows') when it happens.

-- | Gets parent dir of the path.
--
-- Either removes last entry in the path or if there are no entries and just @\"..\/\"@s, adds one more @\"..\/\"@.
--
-- If path is absolute root and it has no parent, it will return unchanged path.
--
-- Examples (pseudocode):
--
-- > parent "a/b/c" == "a/b"
-- > parent "/a" == "/"
-- > parent "/" == "/"
-- > parent "../a/b" == "../a"
-- > parent ".." == "../.."
-- > parent (parent "../a") == "../.."
parent :: Path s b t -> Path s b (Dir d)
parent path = case path of
  ---- System
  RelDir p prefix -> relDirPathParent RelDir P.parent p prefix
  RelFile p prefix -> RelDir (P.parent p) prefix
  AbsDir p -> AbsDir $ P.parent p
  AbsFile p -> AbsDir $ P.parent p
  ---- Windows
  RelDirW p prefix -> relDirPathParent RelDirW PW.parent p prefix
  RelFileW p prefix -> RelDirW (PW.parent p) prefix
  AbsDirW p -> AbsDirW $ PW.parent p
  AbsFileW p -> AbsDirW $ PW.parent p
  ---- Posix
  RelDirP p prefix -> relDirPathParent RelDirP PP.parent p prefix
  RelFileP p prefix -> RelDirP (PP.parent p) prefix
  AbsDirP p -> AbsDirP $ PP.parent p
  AbsFileP p -> AbsDirP $ PP.parent p
  where
    -- NOTE: We need this special logic for RelDir, because if we have RelDir Path,
    --   it is possible that it is "." or smth like that and no parent can be obtained,
    --   in which case we want to add "../" to our prefix.
    --   For file though, we don't have that concern, because it will always be possible to
    --   get a parent, as per current Path implementation.
    relDirPathParent constructor pathParent p prefix =
      if pathParent p == p
        then
          let prefix' = case prefix of
                ParentDir n -> ParentDir (n + 1)
                NoPrefix -> ParentDir 1
           in constructor p prefix'
        else
          let p' = pathParent p
           in constructor p' prefix

-- | Concatenates two paths, same as "FilePath".'FilePath.</>', but only if the second path is relative
-- to the directory that first path leads to, and if both paths use the same path standard.
--
-- How @\"..\/\"@s are resolved (examples are pseudocode):
--
-- - For each @\"..\/\"@ at the start of the right hand path, one most right entry is removed
--   from the left hand path.
--
-- > "a/b" </> "../c" == "a/c"
--
-- - If left path is absolute and right path has too many @"..\/"@s, they go \"over\" the root
--   and are effectively ignored.
--
-- > "/a/b" </> "../../../../../c" == "/c"
--
-- - If left path is relative and right path has more @\"..\/\"@s then left has entries,
--   the leftover @\"..\/\"@s are carried over.
--
-- > "a/b" </> "../../../../../c" == "../../../c"
(</>) :: Path s b (Dir d) -> Path s (Rel d) t -> Path s b t
---- System
lsp@(RelDir _ _) </> (RelFile rp rprefix) =
  let (RelDir lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelFile (lp' P.</> rp) lprefix'
lsp@(RelDir _ _) </> (RelDir rp rprefix) =
  let (RelDir lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelDir (lp' P.</> rp) lprefix'
lsp@(AbsDir _) </> (RelFile rp rprefix) =
  let (AbsDir lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsFile (lp' P.</> rp)
lsp@(AbsDir _) </> (RelDir rp rprefix) =
  let (AbsDir lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsDir (lp' P.</> rp)
---- Windows
lsp@(RelDirW _ _) </> (RelFileW rp rprefix) =
  let (RelDirW lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelFileW (lp' `pathWinCombineRelDirAndRelFile` rp) lprefix'
lsp@(RelDirW _ _) </> (RelDirW rp rprefix) =
  let (RelDirW lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelDirW (lp' `pathWinCombineRelDirAndRelDir` rp) lprefix'
lsp@(AbsDirW _) </> (RelFileW rp rprefix) =
  let (AbsDirW lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsFileW (lp' PW.</> rp)
lsp@(AbsDirW _) </> (RelDirW rp rprefix) =
  let (AbsDirW lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsDirW (lp' `pathWinCombineAbsDirAndRelDir` rp)
---- Posix
lsp@(RelDirP _ _) </> (RelFileP rp rprefix) =
  let (RelDirP lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelFileP (lp' `pathPosixCombineRelDirAndRelFile` rp) lprefix'
lsp@(RelDirP _ _) </> (RelDirP rp rprefix) =
  let (RelDirP lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
   in RelDirP (lp' `pathPosixCombineRelDirAndRelDir` rp) lprefix'
lsp@(AbsDirP _) </> (RelFileP rp rprefix) =
  let (AbsDirP lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsFileP (lp' PP.</> rp)
lsp@(AbsDirP _) </> (RelDirP rp rprefix) =
  let (AbsDirP lp') = iterate parent lsp !! prefixNumParentDirs rprefix
   in AbsDirP (lp' `pathPosixCombineAbsDirAndRelDir` rp)
_ </> _ = impossible

-- | Enables you to redefine which dir is the path relative to.
castRel :: Path s (Rel d1) a -> Path s (Rel d2) a
---- System
castRel (RelDir p pr) = RelDir p pr
castRel (RelFile p pr) = RelFile p pr
---- Windows
castRel (RelDirW p pr) = RelDirW p pr
castRel (RelFileW p pr) = RelFileW p pr
---- Posix
castRel (RelDirP p pr) = RelDirP p pr
castRel (RelFileP p pr) = RelFileP p pr
castRel _ = impossible

-- | Enables you to rename the dir.
castDir :: Path s a (Dir d1) -> Path s a (Dir d2)
---- System
castDir (AbsDir p) = AbsDir p
castDir (RelDir p pr) = RelDir p pr
---- Windows
castDir (AbsDirW p) = AbsDirW p
castDir (RelDirW p pr) = RelDirW p pr
---- Posix
castDir (AbsDirP p) = AbsDirP p
castDir (RelDirP p pr) = RelDirP p pr
castDir _ = impossible

-- | Enables you to rename the file.
castFile :: Path s a (File f1) -> Path s a (File f2)
---- System
castFile (AbsFile p) = AbsFile p
castFile (RelFile p pr) = RelFile p pr
---- Windows
castFile (AbsFileW p) = AbsFileW p
castFile (RelFileW p pr) = RelFileW p pr
---- Posix
castFile (AbsFileP p) = AbsFileP p
castFile (RelFileP p pr) = RelFileP p pr
castFile _ = impossible

-- TODO: I was not able to unite these two functions (`relDirToPosix` and `relFileToPosix`) into just `toPosix``
--   because Haskell did not believe me that I would be returning same "t" (Dir/File) in Path
--   as was in first argument. I wonder if there is easy way to go around that or if
--   we have to redo significant part of the StrongPath to be able to do smth like this.

-- | Converts relative dir path to posix by replacing current path separators with posix path separators.
-- If path is already posix, it will not change.
--
-- Works well for \"normal\" relative paths like @\"a\\b\\c\"@ (Win) or @\"a\/b\/c\"@ (Posix).
-- If path is weird but still considered relative, like just @\"C:\"@ on Win,
-- results can be unexpected, most likely resulting with error thrown.
relDirToPosix :: MonadThrow m => Path s (Rel d1) (Dir d2) -> m (Path Posix (Rel d1) (Dir d2))
relDirToPosix sp@(RelDir _ _) = parseRelDirP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relDirToPosix sp@(RelDirW _ _) = parseRelDirP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relDirToPosix (RelDirP p pr) = return $ RelDirP p pr
relDirToPosix _ = impossible

-- | Converts relative file path to posix, if it is not already posix.
-- Check 'relDirToPosix' for more details, they behave the same.
relFileToPosix :: MonadThrow m => Path s (Rel d1) (File f) -> m (Path Posix (Rel d1) (File f))
relFileToPosix sp@(RelFile _ _) = parseRelFileP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relFileToPosix sp@(RelFileW _ _) = parseRelFileP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relFileToPosix (RelFileP p pr) = return $ RelFileP p pr
relFileToPosix _ = impossible
