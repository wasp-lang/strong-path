{-# LANGUAGE TemplateHaskell #-}

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
-- > defaultUserVlcConfigDir = SP.fromPathRelDir [P.reldir|.config/vlc|]
--
-- where we need to use "Path" library via following import:
--
-- > import qualified Path as P
--
-- and we need QuasiQuotes language extension.
--
-- In the future, "StrongPath" will be able to directly do this, without you needing to additionally import "Path" library, but we haven't implemented this yet.
--
-- == Paths starting with "../"
--
-- Relative paths in "StrongPath" can start with one or multiple "../".
-- "../" is taken into account and appropriately managed when performing operations on paths.
--
-- > someRelPath :: Path System (Rel SomeDir) (File SomeFle)
-- > someRelPath = parseRelFile "../foo/myfile.txt"
--
-- > Currently relative files that start with "../" can't be constructed from string literals in compile time, but support for that will be added in the future.
--
-- == Some more examples
--
-- > -- System path to "foo" directory, relative to "bar" directory.
-- > dirFooInDirBar :: Path System (Rel BarDir) (Dir FooDir)
-- > dirFooInDirBar = fromJust $ fromRelDir "somedir/foo/"
-- >
-- > -- Abs system path to "bar" directory.
-- > dirBarAbsPath :: Path System Abs (Dir BarDir)
-- > dirBarAbsPath = fromJust $ fromAbsDir "/bar/"
-- >
-- > -- Abs path to "foo" directory.
-- > dirFooAbsPath :: Path System Abs (Dir FooDir)
-- > dirFooAbsPath = dirBarAbsPath </> dirFooInDirBar
-- >
-- > -- Posix path to "unnamed" file, relative to "foo" directory.
-- > someFile :: Path Posix (Rel FooDir) File ()
-- > someFile = fromJust $ fromRelFile "some/file.txt"
-- >
-- > dirHome :: Path System Abs (Dir HomeDir)
-- > dirHome :: fromJust $ fromAbsDir "/home/john/"
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

    -- * Parsers (from 'FilePath' to 'Path')
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

    -- * Conversion (from 'Path' to 'FilePath')
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

    -- * Parsers (from "Path".'Path.Path' to 'StrongPath.Path')
    -- $parsersPath
    fromPathRelDir,
    fromPathRelFile,
    fromPathAbsDir,
    fromPathAbsFile,
    fromPathRelDirW,
    fromPathRelFileW,
    fromPathAbsDirW,
    fromPathAbsFileW,
    fromPathRelDirP,
    fromPathRelFileP,
    fromPathAbsDirP,
    fromPathAbsFileP,

    -- * Conversion (from 'StrongPath.Path' to "Path".'Path.Path')
    -- $conversionPath
    toPathRelDir,
    toPathRelFile,
    toPathAbsDir,
    toPathAbsFile,
    toPathRelDirW,
    toPathRelFileW,
    toPathAbsDirW,
    toPathAbsFileW,
    toPathRelDirP,
    toPathRelFileP,
    toPathAbsDirP,
    toPathAbsFileP,

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

    -- * QuasiQuoters
    absdir,
    absdirP,
    absdirW,
    absfile,
    absfileP,
    absfileW,
    reldir,
    reldirP,
    reldirW,
    relfile,
    relfileP,
    relfileW,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadThrow)
import Data.List (intercalate)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Language.Haskell.TH.Syntax as TH
import qualified Path as P
import qualified Path.Posix as PP
import qualified Path.Windows as PW
import StrongPath.Internal
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

-- $parsersPath
-- Functions for parsing "Path" paths into "StrongPath" paths.

-- Constructors
-- TODO: Although here I specify which exact type of Path (P.Path, PP.Path or PW.Path) is to be
--   given as first argument, I realized that if I do:
--     SP.fromPathRelDirW [P.reldir|test\file|]
--   compiler will not complain, although I put P instead of PW!
--   I am not sure why is this happening, we should figure it out.
--   This is not great because it means somebody can by accident construct
--   StrongPath that should be Windows but is really Posix.
--   Or can they? I am not sure if P.Path is just considered the same as PW.Path,
--   or P.relfile and PW.relfile and PP.relfile for some weird reason are polymorhic
--   in return type, or what is happening. I believe it is something close to the latter,
--   in which case it is less of a problem, but I am not sure.
--   Actually, it also does not complain if I do:
--     SP.fromPathRelFileP [P.reldir|test/file|]
--   so although I put reldir, and it should be relfile, it does not complain! How is that possible!?
--   If I put absdir, then it does complain, however not if I put reldir. Very weird.
--   NOTE: In Path, Path.Windows.Path and Path.Posix.Path are actually the same Path it seems
--     so compiler does not differentiate them (because they are all exporting the same module containing Path),
--     but Path.Windows.Rel and Path.Posix.Rel (and same for Abs/Dir/File) are not the same,
--     because they are done via Include mechanism.
fromPathRelDir :: P.Path P.Rel P.Dir -> Path System (Rel a) (Dir b)
fromPathRelFile :: P.Path P.Rel P.File -> Path System (Rel a) (File f)
fromPathAbsDir :: P.Path P.Abs P.Dir -> Path System Abs (Dir a)
fromPathAbsFile :: P.Path P.Abs P.File -> Path System Abs (File f)
fromPathRelDirW :: PW.Path PW.Rel PW.Dir -> Path Windows (Rel a) (Dir b)
fromPathRelFileW :: PW.Path PW.Rel PW.File -> Path Windows (Rel a) (File f)
fromPathAbsDirW :: PW.Path PW.Abs PW.Dir -> Path Windows Abs (Dir a)
fromPathAbsFileW :: PW.Path PW.Abs PW.File -> Path Windows Abs (File f)
fromPathRelDirP :: PP.Path PP.Rel PP.Dir -> Path Posix (Rel a) (Dir b)
fromPathRelFileP :: PP.Path PP.Rel PP.File -> Path Posix (Rel a) (File f)
fromPathAbsDirP :: PP.Path PP.Abs PP.Dir -> Path Posix Abs (Dir a)
fromPathAbsFileP :: PP.Path PP.Abs PP.File -> Path Posix Abs (File f)
---- System
fromPathRelDir p = RelDir p NoPrefix

fromPathRelFile p = RelFile p NoPrefix

fromPathAbsDir = AbsDir

fromPathAbsFile = AbsFile

---- Windows
fromPathRelDirW p = RelDirW p NoPrefix

fromPathRelFileW p = RelFileW p NoPrefix

fromPathAbsDirW = AbsDirW

fromPathAbsFileW = AbsFileW

---- Posix
fromPathRelDirP p = RelDirP p NoPrefix

fromPathRelFileP p = RelFileP p NoPrefix

fromPathAbsDirP = AbsDirP

fromPathAbsFileP = AbsFileP

-- $conversionPath
-- Functions for converting paths from "StrongPath" paths into "Path" paths.

-- TODO: Should I go with MonadThrow here instead of just throwing error? Probably!
--       I could, as error, return actual Path + info on how many ../ were there in StrongPath,
--       so user can recover from error and continue, if they wish.
-- Deconstructors
toPathRelDir :: Path System (Rel a) (Dir b) -> P.Path P.Rel P.Dir
toPathRelFile :: Path System (Rel a) (File f) -> P.Path P.Rel P.File
toPathAbsDir :: Path System Abs (Dir a) -> P.Path P.Abs P.Dir
toPathAbsFile :: Path System Abs (File f) -> P.Path P.Abs P.File
toPathRelDirW :: Path Windows (Rel a) (Dir b) -> PW.Path PW.Rel PW.Dir
toPathRelFileW :: Path Windows (Rel a) (File f) -> PW.Path PW.Rel PW.File
toPathAbsDirW :: Path Windows Abs (Dir a) -> PW.Path PW.Abs PW.Dir
toPathAbsFileW :: Path Windows Abs (File f) -> PW.Path PW.Abs PW.File
toPathRelDirP :: Path Posix (Rel a) (Dir b) -> PP.Path PP.Rel PP.Dir
toPathRelFileP :: Path Posix (Rel a) (File f) -> PP.Path PP.Rel PP.File
toPathAbsDirP :: Path Posix Abs (Dir a) -> PP.Path PP.Abs PP.Dir
toPathAbsFileP :: Path Posix Abs (File f) -> PP.Path PP.Abs PP.File
---- System
toPathRelDir (RelDir p NoPrefix) = p
toPathRelDir (RelDir _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDir _ = impossible

toPathRelFile (RelFile p NoPrefix) = p
toPathRelFile (RelFile _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFile _ = impossible

toPathAbsDir (AbsDir p) = p
toPathAbsDir _ = impossible

toPathAbsFile (AbsFile p) = p
toPathAbsFile _ = impossible

---- Windows
toPathRelDirW (RelDirW p NoPrefix) = p
toPathRelDirW (RelDirW _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDirW _ = impossible

toPathRelFileW (RelFileW p NoPrefix) = p
toPathRelFileW (RelFileW _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFileW _ = impossible

toPathAbsDirW (AbsDirW p) = p
toPathAbsDirW _ = impossible

toPathAbsFileW (AbsFileW p) = p
toPathAbsFileW _ = impossible

---- Posix
toPathRelDirP (RelDirP p NoPrefix) = p
toPathRelDirP (RelDirP _ _) = relativeStrongPathWithPrefixToPathError
toPathRelDirP _ = impossible

toPathRelFileP (RelFileP p NoPrefix) = p
toPathRelFileP (RelFileP _ _) = relativeStrongPathWithPrefixToPathError
toPathRelFileP _ = impossible

toPathAbsDirP (AbsDirP p) = p
toPathAbsDirP _ = impossible

toPathAbsFileP (AbsFileP p) = p
toPathAbsFileP _ = impossible

relativeStrongPathWithPrefixToPathError :: a
relativeStrongPathWithPrefixToPathError =
  error "Relative StrongPath.Path with prefix can't be converted into Path.Path."

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
parseRelDir = parseRelFP RelDir [FP.pathSeparator, FPP.pathSeparator] P.parseRelDir

parseRelFile = parseRelFP RelFile [FP.pathSeparator, FPP.pathSeparator] P.parseRelFile

parseAbsDir fp = fromPathAbsDir <$> P.parseAbsDir fp

parseAbsFile fp = fromPathAbsFile <$> P.parseAbsFile fp

---- Windows
parseRelDirW = parseRelFP RelDirW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelDir

parseRelFileW = parseRelFP RelFileW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelFile

parseAbsDirW fp = fromPathAbsDirW <$> PW.parseAbsDir fp

parseAbsFileW fp = fromPathAbsFileW <$> PW.parseAbsFile fp

---- Posix
parseRelDirP = parseRelFP RelDirP [FPP.pathSeparator] PP.parseRelDir

parseRelFileP = parseRelFP RelFileP [FPP.pathSeparator] PP.parseRelFile

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

-- QuasiQuoters
-- TODO: Split these into a separate module, StrongPath.QuasiQuoters, that will be reexported from this module.
--   This will also need extraction of some other parts of this module, in order to avoid cyclic imports.
-- TODO: Write haddock docs for quasi quoters.
-- TODO: Write tests.

qq ::
  (Lift p, Show err) =>
  (String -> Either err p) ->
  (p -> TH.ExpQ) ->
  QuasiQuoter
qq parse liftP =
  QuasiQuoter
    { quoteExp = either (fail . show) liftP . parse,
      quotePat = err "pattern",
      quoteType = err "type",
      quoteDec = err "declaration"
    }
  where
    err what x = fail ("unexpected " ++ what ++ ", must be expression: " ++ x)

liftPath :: TH.TypeQ -> TH.TypeQ -> TH.TypeQ -> Path s b t -> TH.ExpQ
liftPath s b t p = [|$(lift p) :: Path $s $b $t|]

typeVar :: String -> TH.TypeQ
typeVar = TH.newName >=> TH.varT

absdir, absdirP, absdirW :: QuasiQuoter
absdir = qq parseAbsDir (liftPath [t|System|] [t|Abs|] [t|Dir $(typeVar "d")|])
absdirP = qq parseAbsDirP (liftPath [t|Posix|] [t|Abs|] [t|Dir $(typeVar "d")|])
absdirW = qq parseAbsDirW (liftPath [t|Windows|] [t|Abs|] [t|Dir $(typeVar "d")|])

absfile, absfileP, absfileW :: QuasiQuoter
absfile = qq parseAbsFile (liftPath [t|System|] [t|Abs|] [t|File $(typeVar "f")|])
absfileP = qq parseAbsFileP (liftPath [t|Posix|] [t|Abs|] [t|File $(typeVar "f")|])
absfileW = qq parseAbsFileW (liftPath [t|Windows|] [t|Abs|] [t|File $(typeVar "f")|])

reldir, reldirP, reldirW :: QuasiQuoter
reldir = qq parseRelDir (liftPath [t|System|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])
reldirP = qq parseRelDirP (liftPath [t|Posix|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])
reldirW = qq parseRelDirW (liftPath [t|Windows|] [t|Rel $(typeVar "d1")|] [t|Dir $(typeVar "d2")|])

relfile, relfileP, relfileW :: QuasiQuoter
relfile = qq parseRelFile (liftPath [t|System|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
relfileP = qq parseRelFileP (liftPath [t|Posix|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
relfileW = qq parseRelFileW (liftPath [t|Windows|] [t|Rel $(typeVar "d")|] [t|File $(typeVar "f")|])
