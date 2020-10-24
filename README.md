# StrongPath

[![Build Status](https://travis-ci.com/wasp-lang/strong-path.svg?branch=master)](https://travis-ci.com/wasp-lang/strong-path)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/wasp-lang/strong-path?branch=master&svg=true)](https://ci.appveyor.com/project/Martinsos/strong-path/branch/master)

Strongly typed file paths in Haskell.

```hs
-- Path to "foo" directory, relative to "bar" directory.
dirFooInDirBar :: Path (Rel BarDir) (Dir FooDir)
dirFooInDirBar = fromPathRelDir [P.reldir|somedir/foo/|]

-- Abs path to "bar" directory.
dirBar :: Path Abs (Dir BarDir)
dirBar = fromPathAbsDir [P.absdir|/bar/|]

-- Abs path to "foo" directory.
dirFoo :: Path Abs (Dir FooDir)
dirFoo = dirBar </> dirFooInDirBar

data BarDir -- Represents Bar directory.
data FooDir -- Represents Foo directory.
```

This library is greatly inspired by [path library](https://github.com/commercialhaskell/path) and is really a layer on top of it, replicating most of its API and using it for implementation details, while also adding to it, with main addition being **support at type level for describing what are relative paths exactly relative to**.

## Motivation

While working with file paths in Haskell (using `FilePath`), I had hard time tracking which path is relative, which is absolute, is it a file or a directory, since `FilePath` is just type alias for `String`.

I then started using awesome [path library](https://github.com/commercialhaskell/path), which encodes this information in types: is path absolute or relative, is it file or dir, so we can know at compile time what we are dealing with!

However, while `path` was great improvement over `FilePath`, there was still one piece of information that I kept being "unsure" about: if path is relative, what is it relative to?
Sometimes this is not important and relative paths are not relative to anything specific, but I was dealing with a lot relative paths that were relative to specific directories, and I wanted to be sure when I am concatenating them that I am concatenating the right paths! Or, I would have functions that expect paths relative to some specific dir and I wanted to be sure correct paths are provided as arguments.

This is where `StrongPath` comes in, with additional information being encoded in the type: which dir is the path relative to (if it is relative path), or what is the name of the dir that the path points to (if it is dir path).

## API

NOTE: Since `strong-path` replicates a lot of API from `path`, to avoid confusion, we will prefix everything from `path` with `P`.

### Types

Main type is abstract type Path:
```hs
data Path b t
```
where `b` stands for "base" (relative or absolute) and `t` stands for "type" (file or dir).

For `b` ("base"), we have empty types:
```hs
data Abs
data Rel dir
```

For `t` ("type"), we have empty types:
```hs
data File
data Dir dir
```

This bring us to 4 combinations:
```hs
Path Abs      File
Path Abs      (Dir d)
Path (Rel d)  File
Path (Rel d1) (Dir d2)
```

### Constructors

Path can be constructed from `FilePath`:
```hs
parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs      File)
parseAbsDir  :: MonadThrow m => FilePath -> m (Path Abs      (Dir d))
parseRelFile :: MonadThrow m => FilePath -> m (Path (Rel d)  File)
parseRelDir  :: MonadThrow m => FilePath -> m (Path (Rel d1) (Dir d2))
```

or from `P.Path`:
```hs
fromPathAbsFile :: P.Path P.Abs P.File -> Path Abs     File
fromPathAbsDir  :: P.Path P.Abs P.Dir  -> Path Abs     (Dir a)
fromPathRelFile :: P.Path P.Rel P.File -> Path (Rel a) File
fromPathRelDir  :: P.Path P.Rel P.Dir  -> Path (Rel a) (Dir b)
```

### Unpacking

Path can be unpacked into `FilePath`:
```hs
toFilePath :: Path b t -> FilePath
```

or into `P.Path`:
```hs
toPathAbsFile :: Path Abs     File    -> P.Path P.Abs P.File
toPathAbsDir  :: Path Abs     (Dir a) -> P.Path P.Abs P.Dir
toPathRelFile :: Path (Rel a) File    -> P.Path P.Rel P.File
toPathRelDir  :: Path (Rel a) (Dir b) -> P.Path P.Rel P.Dir
```

### Operations

Main operation is concatenation:
```hs
(</>) :: Path a (Dir d) -> Path (Rel d) c -> Path a c
```
and this is where StrongPath's types come into play: you can concatenate paths only if the second path is relative to the directory that first path leads to.

`parent` gets parent dir of path:
```hs
parent :: Path b t -> Path b (Dir d)
```

`castRel` enables you to redefine which dir is the path relative to.
```hs
castRel :: Path (Rel d1) a -> Path (Rel2 ds) a
```

## Usage and examples

Main idea is that you name dirs in your paths on type level: `Path Abs (Dir FooDir)`.

You can use any type as a name for the directory, but normally you will want to declare one empty data type per directory:
```hs
data FooDir
data BarDir
```

Here is full usage example:

```hs
import StrongPath (Path, Rel, Dir, fromPathRelDir, fromPathAbsDir, (</>), castRel)
import qualified Path as P

dirFooInDirBar :: Path (Rel BarDir) (Dir FooDir)
dirFooInDirBar = fromPathRelDir [P.reldir|somedir/foo/|]

dirBarAbsPath :: Path Abs (Dir BarDir)
dirBarAbsPath = fromPathAbsDir [P.absdir|/bar/|]

dirFooAbsPath :: Path Abs (Dir FooDir)
dirFooAbsPath = dirBar </> dirFooInDirBar

dirHome :: Path Abs (Dir HomeDir)
dirHome :: fromPathAbsDir [P.absdir|/garfield/home/|]

dirFooCopiedToHomeAsInBar :: Path Abs (Dir FooDir)
dirFooCopiedToHomeAsInBar = dirHome </> castRel dirFooInDirBar

data BarDir
data FooDir
data HomeDir
```


## TODO
- [ ] Add support for defining paths in template haskell (same as Path has with reldir[|...|] and similar), so that `path` is not mandatory in order to use StrongPath.
