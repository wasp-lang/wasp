# Waspc

This directory contains source code of the `wasp` compiler (aka `waspc`), and this README is aimed at the contributors to the project.

If you are a Wasp user and not a contributor (yet :)), you might want to look into following resources instead ([**Project page**](https://wasp-lang.dev), [**Docs**](https://wasp-lang.dev/docs)).


## First time contributor checklist
If you would like to make your first contribution, here is a handy checklist we made for you:

- [ ] Read [Quick overview](#quick-overview).
- [ ] Compile the project successfully and get todoApp example running (follow [Basics](#basics)).
- [ ] Join [Discord](https://discord.gg/rzdnErX) and say hi :)!
- [ ] Pick an issue [labeled with "good first issue"](https://github.com/wasp-lang/wasp/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) and let us know you would like to work on it - ideally immediatelly propose a plan of action and ask questions.
      If you can't find a suitable issue for you, reach out to us on Discord and we can try to find smth for you together.
- [ ] Make a PR and have it accepted! Check [Typical workflow](#typical-development-workflow) for guidance, and consult [Codebase overview](#codebase-overview) for more details on how Wasp compiler works internally.


## Quick overview
Wasp compiler is implemented in Haskell, but you will also see a lot of Javascript and other web technologies because Wasp compiler transpiles Wasp code into them.

You don't have to be expert in Haskell to contribute or understand the code, since we don't use complicated Haskell features much -> most of the code is relatively simple and straight-forward, and we are happy to help with the part that is not.

Main result of building the project is `wasp` executable (also reffered to as CLI), which is both Wasp compiler, CLI and Wasp project runner in one - one tool for everything Wasp-related.

`wasp` executable takes `.wasp` files and `ext/` dir as input and generates a web app from them.

![Wasp compilation](images/wasp-diagram.png)

It can then also run that web app for you, deploy it (not yet but that is coming), and manage it in other ways.


## Basics
### Setup
We use [Stack](https://docs.haskellstack.org/en/stable/README/) for building the project, so you will need to install `stack` on your machine.

### Repo
Fork this repo and clone the fork to your machine (or clone this repo directly if you don't plan to contribute but just want to try it out).

Position yourself in this directory (`waspc/`) and make sure that you are on the `master` branch.

### Build
```
stack build
```
to build the library and `wasp` executable.

This might take a longer time (10 mins) if you are doing it for the very first time, since `stack` will need to download the external dependencies.

### Test
```
stack test
```
to ensure all the unit tests are passing (this will also build the project if needed).

### Executable
```
stack exec wasp
```
to run the `wasp` executable that you just built!
It should print "Usage" information.

You can pass more arguments by just adding them to the command, e.g.: `stack exec wasp new MyProject`.

### Run example app
Position yourself in `waspc/examples/todoApp/` and run
```
stack exec wasp db migrate-up
```
to update database schema (this is done only on schema changes).

Then,
```
stack exec wasp start
```
to run web app in development mode.

If you are doing this for the very first time, it might take a minutes or so to download and install npm dependencies.

When done, new tab in your browser should open and you will see a Todo App!
NOTE: Reload page if blank.


## Typical development workflow
1. Create a new feature branch from `master`.
2. Run `run ghcid`: this will run a process that watches the project and automatically rebuilds it on any changes, reporting any compiler errors.
   Leave it running.  
   NOTE: You will need to install `ghcid` globally first, you can do it with `stack install ghcid`.
3. Do a change in the codebase (most often in `lib/` or `cli/`) (together with tests if that makes sense: see "Tests").
   Fix any errors shown by `ghcid`.
   Rinse and repeat.
4. Once close to done, run `stack test` to confirm that project is passing tests (new and old).
5. If needed, confirm that `examples/todoApp/` is working correctly by running `wasp start` from it and manually testing it.
   In the future we will add automatic integration tests, but for now it is manual.
6. When all is ready, squash commits into one commit (or a few if that makes sense) and create a PR. 
   Keep an eye on CI tests -> they should all be passing, if not, look into it.
7. If your PR changes how users(Waspers) use Wasp, make sure to also create a PR that will update the documentation, which is in a [separate repo](https://wasp-lang.dev/docs/tutorials/getting-started).
8. Work with reviewer(s) to get the PR approved.
   Keep adding fix commits until PR is approved, then again squash them all into one commit.
9. Reviewer will merge the branch into `master`. Yay!


## Codebase overview
Wasp is implemented in Haskell.

Codebase is split into library (`src/`) and CLI (`cli/`).
CLI is actually `wasp` executable, and it uses the library, where most of the logic is.

Wasp compiler takes .wasp files + everything in the `ext/` dir (JS, HTML, ...) and generates a web app that consists of client, server and database.

Wasp compiler code is split into 2 basic layers: Parser and Generator.

Wasp file(s) are parsed by Parser into an AST (Abstract Syntax Tree) described in `src/Wasp.hs`.
Parser is implemented via parser combinators and has no distinct tokenization, syntax and semantic analysis steps, currently it is all one big step.

AST generated by Parser is passed to the Generator, which based on it decides how to generate a web app.
Output of Generator is a list of FileDrafts, where each FileDraft explains how to create a file on the disk.
Therefore, Generator doesn't generate anything itself, instead it provides instructions (FileDrafts) on how to generate the web app.
FileDrafts are using mustache templates a lot (they can be found in `data/Generator/templates`).

Generator is split into three generators, for the three main parts of the web app: WebAppGenerator, ServerGenerator and DbGenerator.

After Generator provides us with FileDrafts, web app is generated by writing the FileDrafts to the disk.

![Waspc implementation diagram](images/waspc-implementation-diagram.png)

Generated web app consists of client, server and database.

Client is written with React and react-query.
Server is written in NodeJS and uses ExpressJs.
Database is abstracted via Prisma.

We can run Wasp project with `wasp start`.
This will first compile the app, generate JS code in the `.wasp/out/` dir, and then run `npm start` for the client, `npm start` for the server, and also run the database.
On any changes you do to the source code of Wasp, Wasp project gets recompiled, and then changes in the generated code are picked up by the `npm start` of the client/server, therefore updating the web app.


## Important directories (in waspc/)
- src/ -> main source code, library
- cli/ -> rest of the source code, cli, uses library
- test/ -> tests
- data/Generator/templates/ -> mustache templates for the generated client/server.
- examples/ -> example apps


## Building / development (detailed)
Some useful stack commands:
- `stack build` to build the project, including `wasp` binary which is both CLI and compiler in one.
- `stack exec wasp <arguments>` to run the `wasp` binary that you have built.
- `stack test` to build the whole project + tests and then also run tests.
- `stack build --file-watch` -> live watch, reruns every time a file changes. But we prefer using `ghcid`, it is faster.
- `stack build --pedantic` -> sets -Wall and -Werror ghc options.
- `stack build --profile`
- `stack build --trace`
- `stack install` -> builds the project and then copies it to the local path.
- `stack ghci` -> opens ghci in the context of the project, allowing you to load and run local modules.
- `stack clear` -> clear all generated files/artifacts.

For live compilation and error checking of your code we recommend using `ghcid`.
You can install it globally with `stack install ghcid` and then just type `ghcid --command=stack ghci` when in the project -> it will watch for any file changes and report errors.

### Run script
For more convenient running of common build/dev commands, we created `run` script.
It mostly runs stack commands described above, reducing the number of characters you have to type to run certain commands.

The idea is that you normally use this for development, and you use `stack` directly when you need more control.
It is up to you, using `stack` directly is also perfectly fine and sometimes easier.

You can run `./run help` to learn how to use it.

Examples:
 - `./run ghcid-test` will run ghcid that watches tests, while passing correct arguments to ghcid.


## Tests
For tests we are using [**Tasty**](https://documentup.com/feuerbach/tasty) testing framework. Tasty let's us combine different types of tests into a single test suite.

In Tasty, there is a main test file that is run when test suite is run. In that file we need to manually compose test tree out of tests that we wrote. We organize tests in test groups, which are then recursively grouped resulting in a test tree.
Cool thing is that we can organize tests this way however we want and also mix different type of tests (hspec, quickcheck, and whatever else we want).

Tests are normally split in files of course, so we need to import those all the way up to the main test file, however we organize our test groups/trees.

In order to avoid need for manual organization and importing of test files described above, we are using [tasty-discover](https://hackage.haskell.org/package/tasty-discover) which does this for us.
It automatically detects files containing tests and organizes them for us into a test tree (and also takes care of importing).
This means we only need to create a file, write tests in it and that is it.
Test functions however do need to be prefixed with special prefix to indicate which type of test are they: spec_ for Hspec, prop_ for QuickCheck and similar.
Check docs for more details.
We can however still organize tests manually if we want in Tasty test trees, and then we just prefix them with test_ and tasty-discover will pick them up from there.

Additionally, currently we limited tasty-discover to auto-detect only files ending with Test.hs (*Test.hs glob).
We might remove that requirement in the future if it proves to have no benefit.

To summarize: If you are writing new tests, just put them in a file that ends with `Test.hs` in `test/` dir and that is it.

For unit testing, we use **Hspec**.

For property testing, we use **Quickcheck**.

We additionally use **doctest** for testing code examples in documentation.

All tests go into `test/` directory.
This is convention for Haskell, opposite to mixing them with source code as in Javascript for example.
Not only that, but Haskell build tools don't have a good support for mixing them with source files, so even if we wanted to do that it is just not worth the hassle.

Tests are run with `stack test`.
You can do `stack test --coverage` to see the coverage.

We don't yet have any integration (e2e) tests, but we plan to add them at some point.
For now, best way is to manually run a Wasp app with `wasp start` and try stuff out.


## Linting
While no linter is currently enforced, we recommend using Hlint as a linter.

If using Hlint as a linter, be aware that Hlint doesn't know which default extensions are we using via Stack/cabal, so it might be missing some extension and therefore report false errors.

Hlint already adds a lot of extensions on its own so this is not a very often problem, but if that happens, add default extensions to .hlint.yaml so that Hlint knows to use them.


## Deployment / CI
Every commit is built and tested on Travis (linux, osx) and AppVeyor (win).

If commit is tagged, github draft release is created from it containing binary packages.

If you put `[skip ci]` in commit message, that commit will be ignored by both Travis and AppVeyor.

NOTE: If building of your commit is suddenly taking much longer time, it might be connected with cache on Travis/AppVeyor.


## Documentation
External documentation, for users of Wasp, is hosted in [separate repo](https://wasp-lang.dev/docs/tutorials/getting-started).

Make sure to update it when changes modify how Wasp works.


## Other
Wasp ascii art used in the title is from https://www.asciiart.eu/animals/insects/bees, author: Stef00.
