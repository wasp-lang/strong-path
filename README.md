# StrongPath

[![Build Status](https://travis-ci.com/wasp-lang/strong-path.svg?branch=master)](https://travis-ci.com/wasp-lang/strong-path)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/wasp-lang/strong-path?branch=master&svg=true)](https://ci.appveyor.com/project/Martinsos/strong-path/branch/master)

Strongly typed file paths in Haskell.

```hs
-- System path to "foo" directory, relative to "bar" directory.
dirFooInDirBar :: Path System (Rel BarDir) (Dir FooDir)
dirFooInDirBar = fromPathRelDir [P.reldir|somedir/foo/|]

-- Abs system path to "bar" directory.
dirBar :: Path System Abs (Dir BarDir)
dirBar = fromPathAbsDir [P.absdir|/bar/|]

-- Abs path to "foo" directory.
dirFoo :: Path System Abs (Dir FooDir)
dirFoo = dirBar </> dirFooInDirBar

-- Posix path to "unnamed" file, relative to "foo" directory.
someFile :: Path Posix (Rel FooDir) File ()
someFile = fromPathRelFile [P.relfile|some/file.txt|]

data BarDir -- Represents Bar directory.
data FooDir -- Represents Foo directory.
```

This library is greatly inspired by [path library](https://github.com/commercialhaskell/path) and is really a layer on top of it, replicating most of its API and using it for implementation details, while also adding to it, with main additions being:
 - Differentiation between path standards (system, posix and windows) at type level, they can't be accidentally mixed.
 - "Naming" of directories and files at type level.
 - Support at type level for describing what are relative paths exactly relative to, so you e.g. can't concatenate wrong paths.

## Motivation

While working with file paths in Haskell (using `FilePath`), I had hard time tracking which path is relative, which is absolute, is it a file or a directory, since `FilePath` is just type alias for `String`.

I then started using awesome [path library](https://github.com/commercialhaskell/path), which encodes this information in types: is path absolute or relative, is it file or dir, so we can know at compile time what we are dealing with!

However, while `path` was great improvement over `FilePath`, there was still a piece of information that I kept being "unsure" about: if path is relative, what is it relative to?  
Sometimes this is not important and relative paths are not relative to anything specific, but we were dealing with a lot of relative paths that were relative to very specific directories, and I wanted to be sure when I am concatenating them that I am concatenating the right paths!
Or, I would have functions that expect paths relative to some specific dir and I wanted to be sure correct paths are provided as arguments.

Another problem was the difference between system, windows and posix paths.
While you can't mix windows with posix path in `path` library, you can mix system with windows if OS is Windows (same behaviour for posix). This means that your code could be compiling on Windows, but not on Linux, and vice versa.  
I found that I prefer being very explicit about the standard that specific path is using and not allowing mixing.

This is where `StrongPath` comes in, with additional information being encoded in the type: which dir is the path relative to (if it is relative path), what is the name of the dir/file that the path points to, and what is the path standard (system, windows or posix).


## API

NOTE: Since `strong-path` replicates a lot of API from `path`, to avoid confusion, assume following imports are present:
```hs
import qualified Path         as P
import qualified Path.Posix   as PP
import qualified Path.Windows as PW
```
Therefore, we will prefix stuff from `path` with either `P`, `PP` or `PW`.

### Types

Main type in `strong-path` is abstract type `Path`:
```hs
data Path s b t
```
where `s` stands for "standard", `b` stands for "base" and `t` stands for "type".

#### Standard
For `s` ("standard"), we have 3 possible types:
```hs
data System
data Posix
data Windows
```

`System` means that path uses standard of the system it was compiled on -> if it is Linux/OSX or other Posix system, it will be `Posix`, if it is Windows, it will be `Windows`.
You will normally want to use `System` if you are dealing with the paths on the disk of the host OS (where your code is running), for example if user is providing you with the path to the file on the disk that you will be doing something with.
Keep in mind that `System` causes the behaviour of the `Path` to be (system/platform)-dependant.

`Posix` (`/a/b/c`) and `Windows` (`C:\a\b\c`) are (system/platform)-independent: they behave the same regardless of the OS they are running on.
You will normally want to use one of them when you are dealing with paths from some external source, or with paths that have excplicitely fixed standard.
For example, if you are parsing logs which were obtained from the Windows server, you will want to use `Windows` to work with those paths - it doesn't matter if you are running your code on Linux or Windows, it will always work the same and treat them as `Windows`.
Or, if you are parsing or composing Javascript import statements (`import MyComponent from "../components/MyComponent"`), you will want to use `Posix`, because paths in Javascript import statements are following `Posix` standard.

If you are still not sure what to use, go with `System` since that is the most common use case, and you will likely recognize the situation where you need platform-independent behaviour (`Posix`, `Windows`) when it happens.

#### Base
For `b` ("base"), we have 2 possible types:
```hs
data Abs
data Rel dir
```

#### Type
For `t` ("type"), we have 2 possible types:
```hs
data Dir dir
data File file
```

#### Possible combinations
This bring us to (3x2x2=12) combinations:
```hs
Path [System|Posix|Windows] [Abs|(Rel r)] [(Dir d)|(File f)]
```

#### Naming
When "naming" the files/dirs, you can use any type:
```hs
Path System Abs (Dir Int)
Path System (Rel String) (File (Int, Int))
```
but normally that doesn't make much sense.  
Instead, we recommend sticking with empty data types that you defined yourself:
```hs
data ProjectRootDir
data ProjectManifestFile

Path System Abs (Dir ProjectRootDir)
Path System (Rel ProjectRootDir) (File ProjectManifestFile)
```

Sometimes, if you don't want to specify the meaningful name, it might make sense to go with the `()` instead:
```hs
Path System (Rel ProjectRootDir) (File ())
```

#### Aliases
`StrongPath` defines a couple of convinient aliases for you:
```hs
type Path' = Path System
type Rel'  = Rel ()
type Dir'  = Dir ()
type File' = File ()
```
so instead of writing
```hs
Path System (Rel ProjectRootDir) (File ())
```
you can write
```hs
Path' (Rel ProjectRootDir) File'
```

### Function naming
In strong-path, you will find groups of (usually 12) functions that all do the same thing really but each one of them is specialized for specific type of path.

In such case, we usually name them via following scheme: `<function_name_prefix><base><type><standard>`, where
- `<base>` can be `Rel` or `Abs`.
- `<type>` can be `File` or `Dir`.
- `<standard>` can be `P` (Posix), `W` (Windows) or nothing (System).

This results in 12 functions, for all 12 combinations of path type.

Examples:
- `parseAbsFile` does something with `Path System Abs (File f)`
- `parseRelFileP` does something with `Path Posix (Rel r) (File f)`
- `parseRelDirW` does something with `Path Windows (Rel r) (Dir d)`

### Constructors
#### From FilePath
Path can be constructed from `FilePath`:
```hs
parse<base><type><standard> :: MonadThrow m => FilePath -> m (<corresponding_path_type>)
-- Examples (there are 12 functions in total):
parseAbsFile :: MonadThrow m => FilePath -> m (Path System  Abs      (File f))
parseRelDirW :: MonadThrow m => FilePath -> m (Path Windows (Rel d1) (Dir d2))
```

`parse` functions don't all accept the same path standard as input, e.g. `parseRelDirW` is ok with both Windows and Posix separators in the input (`a\b/c` is OK) while `parseRelDirP` accepts only Posix separators (`a\b` is NOT OK, but `a/b` is OK).  
Here is exact table of which `parse` function takes what input:
```hs
--       Parsers              From          To
-- parseRel[Dir|File]     System/Posix    System
-- parseRel[Dir|File]W    Win/Posix       Win
-- parseRel[Dir|File]P    Posix           Posix
-- parseAbs[Dir|File]     System/Posix*   System
-- parseAbs[Dir|File]W    Win/Posix*      Win
-- parseAbs[Dir|File]P    Posix           Posix
-- 
-- NOTE: * in System/Posix* / Win/Posix* means that while separators
--   can be both System and Posix / Win and Posix, root can't be
--   Posix, it has to instead be System / Win.
```

Basically, all of the parsers accept their "native" standard AND Posix, which enables you to hardcode paths (as Posix) in the code that will compile and work both on Linux and Windows when using `System` as a standard. So Posix becames as a kind of "universal" language.

#### From Path (path library)
Path can also be constructed from `P.Path`:
```hs
from<base><type><standard> :: <corresponding_Path.path_type> -> <corresponding_path_type>
-- Examples: (there are 12 functions in total):
fromPathAbsFile :: P.Path  P.Abs  P.File -> Path System  Abs     (File f)
fromPathRelDirW :: PW.Path PW.Rel PW.Dir -> Path Windows (Rel a) (Dir b)
```

### Unpacking
#### To FilePath
Path can be unpacked into `FilePath` via polymorphic function:
```hs
toFilePath :: Path s b t -> FilePath
```

or via any of the 12 functions that accept specific path type:
```hs
from<base><type><standard> :: <corresponding_path_type> -> FilePath
-- Examples: (there are 12 functions in total):
fromAbsFile :: Path System Abs     (File f) -> FilePath
fromRelDirP :: Path Posix  (Rel r) (Dir d)  -> FilePath
```
We recommend using specific functions instead of `toFilePath`, because that way you are explicit about which path you expect and if that expectancy is not met, type system will catch it.

#### To Path (path library)
Path can also be unpacked into `P.Path`:
```hs
toPath<base><type><standard> :: <corresponding_path_type> -> <corresponding_Path.path_type>
-- Examples: (there are 12 functions in total):
toPathAbsDir   :: Path System  Abs     (Dir a)  -> P.Path  P.Abs  P.Dir
toPathRelFileW :: Path Windows (Rel a) (File f) -> PW.Path PW.Rel PW.File
```

### Operations
Main operation is concatenation:
```hs
(</>) :: Path s b (Dir d) -> Path s (Rel d) t -> Path s b t
```
and this is where StrongPath's types come into play: you can concatenate paths only if the second path is relative to the directory that first path leads to! Also, you can't mistakenly contatenate two paths that are not of the same standard.

`parent` gets parent dir of path:
```hs
parent :: Path s b t -> Path s b (Dir d)
```

`castRel` enables you to redefine which dir is the path relative to.
```hs
castRel :: Path s (Rel d1) a -> Path s (Rel d2) a
```

`castDir` enables you to rename the dir.
```hs
castDir :: Path s b (Dir d1) -> Path s b (Dir d2)
```

`relDirToPosix` transforms relative directory into posix, if not already.
```hs
relDirToPosix :: MonadThrow m => Path s (Rel r) (Dir d) -> m (Path Posix (Rel r) (Dir d))
```

`relFileToPosix` transforms relative file into posix, if not already.
```hs
relFileToPosix :: MonadThrow m => Path s (Rel r) (File f) -> m (Path Posix (Rel r) (File f))
```

## Usage and examples

```hs
import StrongPath (Path, Rel, Dir, System, fromPathRelDir, fromPathAbsDir, (</>), castRel)
import qualified Path as P

dirFooInDirBar :: Path System (Rel BarDir) (Dir FooDir)
dirFooInDirBar = fromPathRelDir [P.reldir|somedir/foo/|]

dirBarAbsPath :: Path System Abs (Dir BarDir)
dirBarAbsPath = fromPathAbsDir [P.absdir|/bar/|]

dirFooAbsPath :: Path System Abs (Dir FooDir)
dirFooAbsPath = dirBar </> dirFooInDirBar

dirHome :: Path System Abs (Dir HomeDir)
dirHome :: fromPathAbsDir [P.absdir|/garfield/home/|]

dirFooCopiedToHomeAsInBar :: Path System Abs (Dir FooDir)
dirFooCopiedToHomeAsInBar = dirHome </> castRel dirFooInDirBar

data BarDir
data FooDir
data HomeDir
```

## TODO
- [ ] Add support for defining paths in template haskell (same as Path has with reldir[|...|] and similar), so that `path` is not mandatory in order to use StrongPath and becomes only an implementation details.

## Contributing / development
`strong-path` is `Stack` project, so make sure you have `stack` installed on your machine.

`stack build` to build the project, `stack test` to run the tests.

`stack build --haddock --no-haddock-deps` to build documentation (without building it for dependencies).  
`stack haddock --open` to open the built documentation.

`stack sdist` to build publishable .tar.gz.
