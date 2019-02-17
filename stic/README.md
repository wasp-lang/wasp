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

Run `stack setup` in the project root to do initial setup. Or, you can just run `stack build` and it will also run `stack setup` as part of it.

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
and run `stack build`.

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


## Tests
For tests we are using [**Tasty**](https://documentup.com/feuerbach/tasty) testing framework. Tasty let's us combine different types of tests into a single test suite.

For unit testing, we use **Hspec**.

For property testing, we use **Quickcheck**.

We additionally use **doctest** for testing code examples in documentation.

Test are run with `stack test`. You can do `stack test --coverage` to see the coverage.

## Benchmarking
For benchmarking we are using [**Criterion**](http://www.serpentine.com/criterion/).

You can run benchmark with `stack bench`.
