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

However, while `path` was great improvement over `FilePath`, there was still one piece of information that I kept being "unsure" about: if path is relative, what is it relative to?  
Sometimes this is not important and relative paths are not relative to anything specific, but we were dealing with a lot of relative paths that were relative to specific directories, and I wanted to be sure when I am concatenating them that I am concatenating the right paths! Or, I would have functions that expect paths relative to some specific dir and I wanted to be sure correct paths are provided as arguments.

Another problem was mixing of system, windows and posix paths.
While you can't mix windows with posix path in `path` library, you can mix system with windows if OS is Windows (same behaviour for posix). This means that your code could be compiling on Windows, but not on Linux, and vice versa.  
I found that by being explicit about the standard that path is using

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

For `s` ("standard"), we have 3 possible types:
```hs
data System
data Posix
data Windows
```

For `b` ("base"), we have 2 possible types:
```hs
data Abs
data Rel dir
```

For `t` ("type"), we have 2 possible types:
```hs
data Dir dir
data File file
```

This bring us to (3*2*2=12) combinations:
```hs
Path [System|Posix|Windows] [Abs|(Rel r)] [(Dir d)|(File f)]
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
Path can be constructed from `FilePath`:
```hs
parse<base><type><standard> :: MonadThrow m => FilePath -> m (<corresponding_path_type>)
-- Examples (there are 12 functions in total):
parseAbsFile :: MonadThrow m => FilePath -> m (Path System  Abs      (File f))
parseRelDirW :: MonadThrow m => FilePath -> m (Path Windows (Rel d1) (Dir d2))
```

or from `Path`:
```hs
from<base><type><standard> :: <corresponding_Path.path_type> -> <corresponding_path_type>
-- Examples: (there are 12 functions in total):
fromPathAbsFile :: P.Path  P.Abs  P.File -> Path System  Abs     (File f)
fromPathRelDirW :: PW.Path PW.Rel PW.Dir -> Path Windows (Rel a) (Dir b)
```

### Unpacking
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

TODO: Aliases

## Usage and examples

Main idea is that you name dirs/files in your paths at type level: `Path System Abs (Dir FooDir)`.

You can use any type as a name for the directory/file, but normally you will want to declare one empty data type per directory/file:
```hs
data FooDir
data BarFile
```

Here is full usage example:

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
- [ ] Add support for defining paths in template haskell (same as Path has with reldir[|...|] and similar), so that `path` is not mandatory in order to use StrongPath.
