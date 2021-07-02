# StrongPath

<p align=center>
  <a href="https://github.com/wasp-lang/strong-path/actions"><img alt="build status" src="https://img.shields.io/github/workflow/status/wasp-lang/strong-path/CI"/></a>
</p>

Strongly typed file paths in Haskell.

This library provides a strongly typed representation of file paths, providing more safety during compile time while also making code more readable, compared to the standard solution (`FilePath`, which is really just `String`).

Without `StrongPath`:
```hs
getGitConfigPath :: IO FilePath
generateHtmlFromMarkdown :: FilePath -> IO FilePath
```

With `StrongPath`:
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

Check [documentation](https://hackage.haskell.org/package/strong-path/docs/StrongPath.html) for more details!

## Documentation
Detailed documentation, including rich examples and API is written via Haddock.

Check out the latest documentation on Hackage: [Documentation](https://hackage.haskell.org/package/strong-path/docs/StrongPath.html).

You can also build and view the Haddock documentation yourself if you wish, by running `stack haddock --open`.

## Contributing / development
`strong-path` is `Stack` project, so make sure you have `stack` installed on your machine.

`stack build` to build the project, `stack test` to run the tests.

`stack build --file-watch --haddock` to rebuild documentation as you change it.

`stack sdist` to build publishable .tar.gz.
