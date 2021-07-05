module StrongPath
  ( -- * Overview

    -- | This library provides a strongly typed representation of file paths, providing more safety during compile time while also making code more readable, compared to the standard solution ("System.FilePath").
    --
    -- Example of using "System.FilePath" vs using "StrongPath" to describe the path to git config file (relative to the home directory):
    --
    -- > getBashProfile :: IO FilePath
    --
    -- > getBashProfile :: IO (Path System (Rel HomeDir) (File BashProfile))
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

    -- ** Function naming

    -- | In "StrongPath" you will find groups of (usually 12) functions that all do the same thing really
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

    -- ** Common examples

    -- | Below we will go through most important features of "StrongPath" by going through some simple code examples that build upon each other.

    -- *** Typical import

    -- |
    -- > import StrongPath (Path, System, Abs, Rel, File, Dir, (</>))
    -- > import qualified StrongPath as SP

    -- *** Absolute path to home dir

    -- |
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
    -- For the simplicity we didn't handle error properly and just used 'Data.Maybe.fromJust',
    -- but normally you would probably want to do something more fancy.

    -- *** Relative path to .gitconfig

    -- |
    -- Next, let's write a function that asks user for a relative path to .gitconfig file in their home directory.
    --
    -- > data UserGitConfig
    -- >
    -- > getUserGitConfigPath :: IO (Path System (Rel HomeDir) (File UserGitConfig))
    -- > getUserGitConfigPath = getLine >>= fromJust . SP.parseRelFile

    -- *** Absolute path to .gitconfig

    -- |
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

    -- *** Copying .gitconfig

    -- |
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

    -- *** Extracting @from@ path from a JS import statement.

    -- |
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

    -- *** Defining a path via string literal during compile time

    -- |
    -- Let's say we want to define default file path from user's home directory to user's VLC config directory, and we already know it while writing our program.
    -- With "StrongPath", we could do it like this:
    --
    -- > defaultUserVlcConfigDir :: Path System (Rel UserHomeDir) (Dir UserVlcConfigDir)
    -- > defaultUserVlcConfigDir = [SP.reldir|.config/vlc|]
    --
    -- where we need QuasiQuotes language extension for 'SP.reldir' quasi quoter to work.
    -- This will parse the path during compile-time, ensuring it is valid.

    -- *** Paths starting with "../"

    -- |
    -- Relative paths in "StrongPath" can start with one or multiple "../".
    -- "../" is taken into account and appropriately managed when performing operations on paths.
    --
    -- > someRelPath :: Path System (Rel SomeDir) (File SomeFle)
    -- > someRelPath = [SP.relfile|../foo/myfile.txt|]

    -- *** Some more examples

    -- |
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

    -- ** Inspiration

    -- |
    -- This library is greatly inspired by [path library](https://github.com/commercialhaskell/path)
    -- and is really a layer on top of it, replicating most of its API and using it for implementation
    -- details, while also adding to it, with main additions being:
    --
    -- - Differentiation between path standards (system, posix and windows) at type level, they can't be accidentally mixed.
    -- - \"Naming\" of directories and files at type level.
    -- - Support at type level for describing what are relative paths exactly relative to,
    --   so you e.g. can't concatenate wrong paths.
    -- - Support for @..\/@ at start of relative path.

    -- * API
    module StrongPath.Types,
    module StrongPath.FilePath,
    module StrongPath.Operations,
    module StrongPath.TH,

    -- ** Working with "Path" library

    -- | If you are using "Path" library alongside "StrongPath", you can import module "StrongPath.Path",
    -- which contains functions for converting "StrongPath" 'Path' into 'Path.Path' and vice versa.
  )
where

import StrongPath.FilePath
import StrongPath.Operations
import StrongPath.TH
import StrongPath.Types
