<p align=center>
  <img height="80px" src="https://user-images.githubusercontent.com/1536647/77317442-78625700-6d0b-11ea-9822-0fb21e557e87.png"/>
</p>

<br>

```
                                                                     _  _
                            __      ____ _ ___ _ __   ___           | )/ )
                            \ \ /\ / / _` / __| '_ \ / __|       \\ |//,' __
                             \ V  V / (_| \__ \ |_) | (__        (")(_)-"()))=-
                              \_/\_/ \__,_|___/ .__/ \___|          (\\
                                              |_|
```

This directory contains source code of Wasp compiler (waspc), and this README is aimed for contributors to the project. If you are a Wasp user and not contributor (yet :)), you might want to look into following resources instead:

- [**Project page**](https://wasp-lang.dev)
- [**Docs**](https://wasp-lang.github.io/web/docs)


## Setup
This is a [Stack](https://docs.haskellstack.org/en/stable/README/) project, so you will need to install `stack` on your machine.

Then run `stack setup` in the project root to do initial setup.

## Building / development

`stack build` to build the project, including `wasp` binary which is both CLI and compiler in one.

`stack exec wasp <arguments>` to run the `wasp` binary that you have built.

`stack test` to build the whole project + tests and then also run tests.

Some other useful `stack` commands:
- `stack build --file-watch` -> live watch, reruns every time a file changes.
- `stack build --pedantic` -> sets -Wall and -Werror ghc options.
- `stack build --profile`
- `stack build --trace`
- `stack install` -> builds the project and then copies it to the local path.
- `stack ghci` -> opens ghci in the context of the project, allowing you to load and run local modules.
- `stack clear` -> clear all generated files/artifacts.

For live compilation and error checking of your code we recommend using `ghcid`.
You can install it globally with `stack install ghcid` and then just type `ghcid --command=stack ghci` when in the project -> it will watch for anyfile changes and report errors.

### Run script
For more convenient running of common build/dev commands, we created `run` script.
It mostly runs stack commands described above, reducing the number of characters you have to type to run certain commands.

The idea is that you normally use this for development, and you use `stack` directly when you need more control.
It is up to you, using `stack` directly is also perfectly fine.

You can run `./run help` to learn how to use it.

Examples:
 - `./run ghcid-test` will run ghcid that watches tests, while passing correct arguments to ghcid.


## Tests
For tests we are using [**Tasty**](https://documentup.com/feuerbach/tasty) testing framework. Tasty let's us combine different types of tests into a single test suite.

In Tasty, there is a main test file that is run when test suite is run. In that file we need to manually compose test tree out of tests that we wrote. We organize tests in test groups, which are then recursively grouped resulting in a test tree.
Cool thing is that we can organize tests this way however we want and also mix different type of tests (hspec, quickcheck, and whatever else want really).

Tests are normally split in files of course, so we need to import those all the way up to the main test file, however we organize our test groups/trees.

In order to avoid need for manual organization and importing of test files described above, we are using [tasty-discover](https://hackage.haskell.org/package/tasty-discover) which does this for us. It automatically detects files containing tests and then organizes them for us into test tree (and also takes care of importing). This means we only need to create a file, write tests in it and that is it.
Test functions however do need to be prefixed with special prefix to indicate which type of test are they: spec_ for Hspec, prop_ for QuickCheck and similar. Check docs for more details.
We can however still organize tests manually if we want in Tasty test trees, and then we just prefix them with test_ and tasty-discover will pick them up from there.

Additionally, currently we limited tasty-discover to auto-detect only files ending with Test.hs (*Test.hs glob). We might remove that requirement in the future if it proves to have no benefit.

For unit testing, we use **Hspec**.

For property testing, we use **Quickcheck**.

We additionally use **doctest** for testing code examples in documentation.

All tests go into `test/` directory. This is convention for Haskell, opposite to mixing them with source code as in Javascript for example. Not only that, but Haskell build tools don't have a good support for mixing them with source files, so even if we wanted to do that it is just not worth the hassle.

Test are run with `stack test`. You can do `stack test --coverage` to see the coverage.

## Benchmarking
For benchmarking we are using [**Criterion**](http://www.serpentine.com/criterion/).

You can run benchmark with `stack bench`.

## Linting

If using Hlint as linter, be aware that Hlint doesn't know which default extensions are we using via Stack/cabal, so it might be missing some extension and therefore report false errors.

Hlint already adds a lot of extensions on its own so this is not a very often problem, but if that happens, add default extensions to .hlint.yaml so that Hlint knows to use them.

## Deployment / CI

Every commit is built and tested on Travis (linux, osx) and AppVeyor (win).

If commit is tagged, github draft release is created from it containing binary packages.

If you put `[skip ci]` in commit message, that commit will be ignored by both Travis and AppVeyor.

NOTE: If building of your commit is suddenly taking much longer time, it might be connected with cache on Travis/AppVeyor.

## Other

Wasp ascii art used in the title is from https://www.asciiart.eu/animals/insects/bees, author: Stef00.
