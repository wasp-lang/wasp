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

## Install
### From binary
To install latest release, run `curl -sSL https://raw.githubusercontent.com/wasp-lang/wasp/master/waspc/tools/install.sh | sh`!

This will work for linux/osx and will install a prebuilt binary. Since the binary is dynamically built (for now), it might not work if you are missing some packages/libraries on your OS or if your OS distro is very different from the one it was built on.

### From source
Install `stack`.

Run `stack setup` in the project root to do initial setup.

Run `stack build` to build the project, including wasp binary.

Run `stack exec wasp <args>` to run wasp binary.


## Project configuration overview
This is a [Stack](https://docs.haskellstack.org/en/stable/README/) project.

### Adding a package as a dependency
This is just so you don't have to search Stack docs.

Just put it in package.yaml like this:
```
dependencies:
  ...
  - <package_name>
  ...
```

If package you need is not in the Stack snapshot defined by `resolver`, add it to `extra-deps` instead of `dependencies`.


## Building / development
You build the project with `stack build`. It uses `package.yaml`, `waspc.yaml` (and possibly some other files in the future) and source files to generate files and build the project.

It is recommended using `stack build --pedantic` to turn on pedantic code checking (-Wall, -Werror).

`stack exec <my-executable>` will run executable in the context of stack project.
In our case, `stack exec waspc-cli` will run waspc (build it first with `stack build`!).

Some useful command options when building:
- `stack build --test` -> same as `stack test`.
- `stack build --file-watch` -> live watch, reruns every time a file changes.
- `stack build --pedantic` -> sets -Wall and -Werror ghc options.
- `stack build --ghc-options="-Wall -Werror"` -> sets ghc options.
- `stack build --bench`
- `stack build --profile`
- `stack build --trace`

There is also `stack install` which builds the project and then copies it to the local bin path.

Running `stack ghci` will open ghci in the context of the project, allowing you to load and run local modules, which can be useful for development.

You can use `stack clear` to clear all the generated files/artifacts from the project.

When developing, if you don't have full support for Haskell in your editor, consider using `stack build --file-watch` for live error checking, or `ghcid` as a faster alternative (install it globally with `stack install ghcid` and then just type `ghcid` when in the project.

### Run script
For more convenient running of common build/dev commands, we created `run` script.
It mostly runs stack commands described above.

The idea is that you normally use this for development, and you use `stack` directly when you need more control.

You can run `./run help` to learn how to use it.

Examples:
 - `./run exec examples/todoMVC.wasp out/todoMVC` will run waspc on todoMVC example.


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

Tagged commits are also deployed as github releases.

If you put `[skip ci]` in commit message, that commit will be ignored by both Travis and AppVeyor.

NOTE: If building of your commit is suddenly taking much longer time, it might be connected with cache on Travis/AppVeyor.

## Other

Wasp ascii art used in the title is from https://www.asciiart.eu/animals/insects/bees, author: Stef00.
