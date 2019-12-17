```
                           ________   _________    ___    ________  
                          |\   ____\ |\___   ___\ |\  \  |\   ____\  
                          \ \  \___|_\|___ \  \_| \ \  \ \ \  \___|  
                           \ \_____  \    \ \  \   \ \  \ \ \  \  
                            \|____|\  \    \ \  \   \ \  \ \ \  \  
                               ___\_\  \    \ \  \   \ \  \ \ \  \____  
                              |\________\    \ \  \   \ \__\ \ \_______\  
                              \|________|     \|__|    \|__|  \|_______|  
```

## Setup
Install `stack`.

Run `stack setup` in the project root to do initial setup.

## Project configuration overview
This is a [Stack](https://docs.haskellstack.org/en/stable/README/) project.

Stack installs GHC and packages automatically, in reproducible manner, in an isolated environment just for your Haskell project.

This project was created from `new-template` stack template and then modified inspired by `haskeleton` stack template.

Most important configuration file is `package.yaml`. Here we define what is what in our project, how is it built. Stack generates `stic.cabal` from this document.

Also important is `stack.yaml` where we define Stack configuration for this project.

We provided some pieces of Stack documentation in this README to make it easier to start with the project, but for any more detailed and guaranteed up-to-date docs check Stack docs at https://docs.haskellstack.org/en/stable/README/.

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
You build the project with `stack build`. It uses `package.yaml`, `stic.yaml` (and possibly some other files in the future) and source files to generate files and build the project.

It is recommended using `stack build --pedantic` to turn on pedantic code checking (-Wall, -Werror).

`stack exec <my-executable>` will run executable in the context of stack project.
In our case, `stack exec stic-exe` will run Stic (build it first with `stack build`!).

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
 - `./run exec examples/todoMVC.wasp out/todoMVC` will run stic on todoMVC example.


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
