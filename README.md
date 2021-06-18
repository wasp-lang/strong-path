# StrongPath

[![Build Status](https://travis-ci.com/wasp-lang/strong-path.svg?branch=master)](https://travis-ci.com/wasp-lang/strong-path)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/wasp-lang/strong-path?branch=master&svg=true)](https://ci.appveyor.com/project/Martinsos/strong-path/branch/master)

Strongly typed file paths in Haskell.

Without StrongPath:
```hs
getGitConfigPath :: IO FilePath
generateHtmlFromMarkdown :: FilePath -> IO FilePath
```

With StrongPath:
```hs
getGitConfigPath :: IO (Path System (Rel HomeDir) (File GitConfigFile))
generateHtmlFromMarkdown :: Path System (Rel HomeDir) (File MarkdownFile) -> IO (Path System Abs (File HtmlFile))
```

Simple but complete example:
```hs
import StrongPath (Path, System, Abs, Rel, File, Dir, (</>), parseAbsDir)

data HomeDir

getHomeDirPath :: IO (Path System Abs (Dir HomeDir))
getHomeDirPath = getLine >>= fromJust . parseAbsDir
```

Check documentation for more details!

## Documentation
Detailed documentation, including examples and API is written via Haddock, inside the source code.

TODO: Send reader to hackage to read the docs for the latest released version there.

You can build and view the Haddock documentation yourself by running `stack haddock --open`.

## Contributing / development
`strong-path` is `Stack` project, so make sure you have `stack` installed on your machine.

`stack build` to build the project, `stack test` to run the tests.

`stack build --file-watch --haddock` to rebuild documentation as you change it.

`stack sdist` to build publishable .tar.gz.
