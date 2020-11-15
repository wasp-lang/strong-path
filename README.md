# StrongPath

[![Build Status](https://travis-ci.com/wasp-lang/strong-path.svg?branch=master)](https://travis-ci.com/wasp-lang/strong-path)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/wasp-lang/strong-path?branch=master&svg=true)](https://ci.appveyor.com/project/Martinsos/strong-path/branch/master)

Strongly typed file paths in Haskell.

TODO: Hackage badge?

## Documentation
Documentation is written as Haddock docs inside the code.

TODO: Send reader to hackage to read the docs for the latest released version there.

You can build and view the Haddock documentation yourself by running `stack haddock --open`.

## TODO
- [ ] Add support for defining paths in template haskell (same as Path has with reldir[|...|] and similar), so that `path` is not mandatory in order to use StrongPath and becomes only an implementation details.

## Contributing / development
`strong-path` is `Stack` project, so make sure you have `stack` installed on your machine.

`stack build` to build the project, `stack test` to run the tests.

`stack build --file-watch --haddock` to rebuild documentation as you change it.

`stack sdist` to build publishable .tar.gz.
