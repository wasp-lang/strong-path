-- | This library provides a strongly typed representation of file paths, providing more safety during compile time while also making code more readable, compared to the standard solution ("System.FilePath").
--
-- Example of using "System.FilePath" vs using "StrongPath" to describe the path to git config file (relative to the home directory):
--
-- > getGitConfigPath :: IO FilePath
--
-- > getGitConfigPath :: IO (Path System (Rel HomeDir) (File GitConfigFile))
--
-- Or, imagine stumbling onto this function:
--
-- > generateHtmlFromMarkdown :: FilePath -> IO FilePath
--
-- What kind of path does it take - relative, absolute? If relative, to what is it relative? What kind of path does it return? Do paths in question follow Posix or Windows standard?
-- With "StrongPath", same function could look like this:
--
-- > generateHtmlFromMarkdown :: Path System (Rel HomeDir) (File MarkdownFile) -> IO (Path System Abs (File HtmlFile))
--
-- Basic idea is that working with 'FilePath' (which is just an alias for String
-- and is a default type for representing file paths in Haskell) is too clumsy
-- and can easily lead to errors in runtime, while those errors could have been caught
-- in the compile time if more advanced approach for representing file paths was used.
--
-- This is where "StrongPath" with its 'Path' type comes in: by encoding
-- more information about the file path into the type (e.g. is it relative or
-- absolute, if it is relative what is it relative to, is it file or dir), we
-- can achieve that additional safety and catch many potential errors during compile time,
-- while also making code more readable.
--
-- Some examples:
--
--  - If you have absolute path to directory on the disk such as @\/home/\john\/Music@,
--    with "StrongPath" you could represent it as @Path System Abs (Dir MusicDir)@,
--    capturing its details in the type.
--
--  - If you have relative (to home) path to file on the disk such as @john\/.gitconfig@,
--    you could represent it as @Path System (Rel HomeDir) (File JohnsGitConfigFile)@.
--
--  - If you have @..\/index.js@ path, coming from the Javascript import statement
--    @import Stuff from \"..\/index.js\"@, you could represent it as
--    @Path Posix (Rel ()) (File IndexFile)@.
--
--
-- Notice that "StrongPath" will not allow you to, for example, represent @\/foo\/bar.txt@, which is an
-- absolute path, as @Path System (Rel SomeDir) (File BarFile)@, because the parser function (in
-- this case 'parseRelFile') will detect that path is absolute and not relative
-- and will throw compile error.
-- Therefore, due to the checks that parser functions perform,
-- once you get 'FilePath' converted into 'Path', you can be pretty sure that it
-- is exactly what the type says it is.
--
-- Once you have your file path represented as 'Path', you can perform safe operations like
-- `</>` (concatenation of two paths) where types really shine.
-- Specifically, `</>` will allow you to concatenate two paths only if they use the same standard,
-- right path is relative to the left path and the left path is a directory.
-- If these conditions are not satisfied, the code will not compile!
--
-- = Function naming
-- In "StrongPath" you will find groups of (usually 12) functions that all do the same thing really
-- but each one of them is specialized for specific type of path.
--
-- In such case, we usually name them via following scheme: @\<function_name_prefix\>\<base\>\<type\>\<standard\>@, where
--
--  - @\<base\>@ can be @Rel@ or @Abs@.
--  - @\<type\>@ can be @File@ or @Dir@.
--  - @\<standard\>@ can be @P@ (Posix), @W@ (Windows) or nothing (System).
--
-- This results in 12 functions, for all 12 combinations of path type.
--
-- For example, from their name, we can say for the following functions that:
--
--  - @parseAbsFile@ does something with @Path System Abs (File f)@
--  - @parseRelFileP@ does something with @Path Posix (Rel r) (File f)@
--  - @parseRelDirW@ does something with @Path Windows (Rel r) (Dir d)@
--
-- = Commmon examples
--
-- Below we will go through most important features of "StrongPath" by going through some simple code examples that build upon each other.
--
-- == Typical import
--
-- > import StrongPath (Path, System, Abs, Rel, File, Dir, (</>))
-- > import qualified StrongPath as SP
--
-- == Absolute path to home dir
--
-- Let's say that you want to ask user for absolute path to their home directory.
-- With "StrongPath", you could do it like this:
--
-- > data HomeDir
-- >
-- > getHomeDirPath :: IO (Path System Abs (Dir HomeDir))
-- > getHomeDirPath = getLine >>= fromJust . SP.parseAbsDir
--
-- Notice how you captured all the important information in type, plus
-- you ensure it is indeed valid path by parsing it (with 'parseAbsDir')!
--
-- For the simplicity we didn't handle error properly and just used 'fromJust',
-- but normally you would probably want to do something more fancy.
--
-- == Relative path to .gitconfig
--
-- Next, let's write a function that asks user for a relative path to .gitconfig file in their home directory.
--
-- > data UserGitConfig
-- >
-- > getUserGitConfigPath :: IO (Path System (Rel HomeDir) (File UserGitConfig))
-- > getUserGitConfigPath = getLine >>= fromJust . SP.parseRelFile
--
-- == Absolute path to .gitconfig
--
-- If user inputed both abs path to home dir and rel path to .gitconfig, we can
-- compute abs path to .gitconfig:
--
-- > absHomeDirPath <- getHomeDirPath
-- > relGitConfigPath <- getUserGitConfigPath
-- > let absGitConfigPath = absHomeDirPath </> relGitConfigPath
--
-- Cool thing here is that you can be sure that @absGitConfigPath@ makes sense, because '</>' would not allow
-- you (at compile time) to concatenate @relGitConfigPath@ with anything else than path to home dir, since it knows
-- that is what it is relative to!
--
-- == Copying .gitconfig
--
-- Let's say that for some reason, we want to copy this .gitconfig to home dir of another user,
-- and we want it to have the same relative position in that home dir as it has in the current home dir.
--
-- Let's assume we already have
--
-- > anotherHomeDir :: IO (Path System Abs (Dir AnotherHomeDir))
--
-- then we can do smth like this:
--
-- > let absAnotherGitConfigPath = anotherHomeDir </> (SP.castRel relGitConfigPath)
--
-- We used 'castRel' to "loosen up" @relGitConfigPath@'s type, so it does not require to be relative
-- to @HomeDir@ and instead accepts @AnotherHomeDir@.
--
-- Similar to 'castRel', there are also 'castFile' and 'castDir'.
--
-- Now we could do the copying like this:
--
-- > copyFile (fromAbsFile absGitConfigPath) (fromAbsFile absAnotherGitConfigPath)
--
-- Notice that while converting 'Path' to 'FilePath', we could have used 'toFilePath' instead of
-- 'fromAbsFile', but 'fromAbsFile' gives us more type safety by demanding given 'Path' to be
-- of specific type (absolute file). For example, if somehow variable @absGitConfigPath@ got to be of type
-- @Path System (Rel ()) (Dir ())@, 'fromAbsFile' would cause compile time error, while 'toFilePath'
-- would just happily go on.
--
-- == Extracting @from@ path from a JS import statement.
--
-- What if we wanted to extract @from@ path from a Javascript import statement and return it as a 'Path'?
--
-- Example of Javascript import statement:
--
-- > import Bar from "../foo/bar"  // We want to extract "../foo/bar" path.
--
-- Let's assume that we know that this statement is relative to some @ProjectDir@ (because that is where the
-- JS file we got the statement from is located), but we don't know upfront the name of the file being imported.
--
-- Such function could have the following signature:
--
-- > parseJsImportFrom :: String -> Maybe (Path Posix (Rel (ProjectDir)) (File ()))
--
-- Notice how we used 'Posix' to specify that the path is following posix standard
-- no matter on which OS we are running this code, while in examples above we
-- used 'System', which meant paths follow whatever is the standard of the OS we are running on.
--
-- Next, also notice how we used @File ()@ to specify that file is \"unnamed\".
-- While you could use some other approach to specify this, we found this to be convenient way to do it.
-- That is why we also introduce @File\'@ and @Dir\'@ aliases, to make this even simpler.
--
-- == Defining a path via string literal during compile time
--
-- Let's say we want to define default file path from user's home directory to user's VLC config directory, and we already know it while writing our program.
-- With "StrongPath", we could do it like this:
--
-- > defaultUserVlcConfigDir :: Path System (Rel UserHomeDir) (Dir UserVlcConfigDir)
-- > defaultUserVlcConfigDir = [SP.reldir|.config/vlc|]
--
-- where we need QuasiQuotes language extension for 'SP.reldir' quasi quoter to work.
-- This will parse the path during compile-time, ensuring it is valid.
--
-- == Paths starting with "../"
--
-- Relative paths in "StrongPath" can start with one or multiple "../".
-- "../" is taken into account and appropriately managed when performing operations on paths.
--
-- > someRelPath :: Path System (Rel SomeDir) (File SomeFle)
-- > someRelPath = [SP.relfile|../foo/myfile.txt|]
--
-- == Some more examples
--
-- > -- System path to "foo" directory, relative to "bar" directory.
-- > dirFooInDirBar :: Path System (Rel BarDir) (Dir FooDir)
-- > dirFooInDirBar = [reldir|somedir/foo|]
-- >
-- > -- Abs system path to "bar" directory.
-- > dirBarAbsPath :: Path System Abs (Dir BarDir)
-- > dirBarAbsPath = [absdir|/bar/|]
-- >
-- > -- Abs path to "foo" directory.
-- > dirFooAbsPath :: Path System Abs (Dir FooDir)
-- > dirFooAbsPath = dirBarAbsPath </> dirFooInDirBar
-- >
-- > -- Posix path to "unnamed" file, relative to "foo" directory.
-- > someFile :: Path Posix (Rel FooDir) File ()
-- > someFile = [relfileP|some/file.txt|]
-- >
-- > dirHome :: Path System Abs (Dir HomeDir)
-- > dirHome :: [absdir|/home/john/|]
-- >
-- > dirFooCopiedToHomeAsInBar :: Path System Abs (Dir FooDir)
-- > dirFooCopiedToHomeAsInBar = dirHome </> castRel dirFooInDirBar
-- >
-- > data BarDir  -- Represents Bar directory.
-- > data FooDir  -- Represents Foo directory.
-- > data HomeDir -- Represents Home directory.
--
-- = Inspiration
-- This library is greatly inspired by [path library](https://github.com/commercialhaskell/path)
-- and is really a layer on top of it, replicating most of its API and using it for implementation
-- details, while also adding to it, with main additions being:
--
-- - Differentiation between path standards (system, posix and windows) at type level, they can't be accidentally mixed.
-- - \"Naming\" of directories and files at type level.
-- - Support at type level for describing what are relative paths exactly relative to,
--   so you e.g. can't concatenate wrong paths.
-- - Support for @..\/@ at start of relative path.
module StrongPath
  ( -- * Types

    -- ** Path
    Path,

    -- *** 'Path' type
    Dir,
    File,

    -- *** 'Path' base
    Abs,
    Rel,

    -- *** 'Path' standard
    -- $pathStandard
    Posix,
    Windows,
    System,

    -- *** 'Path' aliases
    Path',
    Rel',
    Dir',
    File',
    --
    module StrongPath.FilePath,

    -- * Operations
    (</>),
    parent,

    -- * Casting
    castRel,
    castDir,
    castFile,

    -- * Conversion of path standard
    relDirToPosix,
    relFileToPosix,
    --
    module StrongPath.TH,
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.FilePath
import StrongPath.Internal
import StrongPath.TH
import qualified System.FilePath as FP
import qualified System.FilePath.Posix as FPP
import qualified System.FilePath.Windows as FPW

-- TODO: Add relDirToWindows and relFileToWindows?
-- TODO: Implement relFile?

-- TODO: We still depend on Path for creating hardcoded paths via generics. Any way to go around that?
--   Maybe implement our own mechanism for that, so that people don't have to know about / use Path?
--   This means we would implement our own [reldir|foobar|] stuff.

-- TODO: Can I use type classes and return type polymorhipsm to make all this shorter and reduce duplication?
-- class Path, and then I have PathWindows and PathPosix and PathSystem implement it, smth like that?
-- And then fromPathRelDir has polymorhic return type based on standard? I tried a little bit but it is complicated.

-- TODO: If there is no other solution to all this duplication, do some template haskell magic to simplify it.

-- TODO: Extract "Path" parsers and converters into separate StrongPath.Path module, since we don't need them always any more.

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
