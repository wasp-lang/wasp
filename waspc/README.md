# Waspc

This directory contains source code of the `wasp` CLI/compiler (aka `waspc`), and this README is aimed at the contributors to the project.

If you are a Wasp user and not a contributor (yet 😉), you might want to look into the following resources instead ([**Project page**](https://wasp.sh), [**Docs**](https://wasp.sh/docs)).

## First time contributor checklist

If you would like to make your first contribution, here is a handy checklist we made for you:

- [ ] Read [Quick overview](#quick-overview).
- [ ] Compile the project successfully and get todoApp example running (follow [Basics](#basics)).
- [ ] Join [Discord](https://discord.gg/rzdnErX) and say hi :)!
- [ ] Pick an issue [labeled with "good first issue"](https://github.com/wasp-lang/wasp/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) and let us know you would like to work on it - ideally immediately propose a plan of action and ask questions.
      If you can't find a suitable issue for you, reach out to us on Discord and we can try to find something for you together.
- [ ] Make a PR targeting `main` and have it accepted! Check [Typical workflow](#typical-development-workflow) and [Branching and merging strategy](#branching-and-merging-strategy) for guidance, and consult [Codebase overview](#codebase-overview) for more details on how Wasp compiler works internally.

## Quick overview

The core of `waspc` is implemented in Haskell, but you will also see a lot of Javascript/TypeScript and other web technologies which apps generated with Wasp use.

You don't have to be an expert in Haskell to contribute or understand the code since we don't use complicated Haskell features much. Most of the code is relatively simple and straight-forward, and we are happy to help with anything you get stuck with.

The main result of building the project is `wasp` executable (also referred to as CLI), which is at the same time Wasp compiler, CLI, and Wasp project runner in one - one main tool for everything Wasp-related.

Note that while `waspc` is the name of this Haskell project, the executable's name is `wasp` when released, and `wasp-cli` during development.

## Basics

### Repo

Clone this repo (or its fork) to your machine.

Position yourself in this directory (`waspc/`) and make sure that you are on the `main` branch.

### `run` script

[`./run`](run) script captures the most common commands/workflows for the development of `waspc` (similar to the role that `scripts` in `package.json` play in `npm` projects), e.g. `./run build`, `./run test`, ... .

It serves both as a convenient way to easily run common development tasks and also as a sort of documentation on how we develop `waspc`. You will see us mentioning it often in the rest of this README.

Running `./run` without any arguments will print help/usage, which is a good way to discover the typical `waspc` development workflows.

> [!TIP]
> To make it easy to run the `./run` script from any place in your waspc codebase, you can create a bash alias that points to it, e.g.:
>
> ```sh
> alias wrun="/home/martin/git/wasp-lang/wasp/waspc/run"
> ```

### Setup

#### Haskell toolchain

The best way to install the Haskell toolchain is via [ghcup](https://www.haskell.org/ghcup/).

Once you have `ghcup` installed, run the `ghcup tui` command to install and select the correct versions of `cabal` (package manager), `hls` (language server), and `ghc` (compiler).

To find out what the correct versions of these tools are for the `waspc`:

- Run `./run ghcup-set` to determine the specific versions of `ghc` and `hls` you should use. While the primary purpose of this command is to set them to the correct versions, it will also print those versions as a side effect.
- As for `cabal`, just go with the latest one.

> [!NOTE]
> On Mac, we recommend using the official [ghcup](https://www.haskell.org/ghcup/) installer over Homebrew, as it works out of the box.

#### Javascript / TypeScript toolchain

You need to have the correct version of `node` (and `npm`) installed and set.

Check [.nvmrc](.nvmrc) file to learn which version of `node` that should be.

### Build

```sh
./run build:all
```

to build the whole `waspc` project.

This might take a while (e.g. 10 mins) if you are doing it for the very first time, due to having to download all the dependencies (which will later be cached).
If that is the case, relax and feel free to get yourself a cup of coffee! When somebody asks what you are doing, you can finally rightfully say "compiling!" 😀.

> [!NOTE]
> If you are on Mac and get "Couldn't figure out LLVM version!" error message while building, make sure you have LLVM installed and that it is correctly exposed via env vars (PATH, LDFLAGS, CPPFLAGS).
> The easiest way to do it is by just running `brew install llvm@13`, this should install LLVM and also set up env vars.
>
> If the LLVM error persists even after its installation, you may need to manually add it your PATH. To do this, you should add the following to end of your shell rc file (e.g. _~/.bashrc_ or _~/.zshrc_): `export PATH="/opt/homebrew/opt/llvm@13/bin:$PATH"`.

### Run tests

```sh
./run test
```

to ensure all the tests are passing.

> [!NOTE]
> On Windows, the powershell's active code page should be changed to utf-8 to support special letters in some test suits (ref. [PR3027](https://github.com/wasp-lang/wasp/pull/3027)).
>
> ```ps
> chcp 65001
> ```

### Run the `wasp` CLI

```sh
./run wasp-cli
```

to run the `wasp-cli` executable (that you previously built with `./run build:all`)!

Since you provided no arguments, you should see help/usage.

Note that the executable during development is named `wasp-cli`, unlike `wasp` which is how it is named when you install a public release of Wasp via official installation method.
This is to make it easier to differentiate between the development version of wasp CLI and the released version.

### Running the example app

First, position yourself in the [waspc/examples/todoApp/](examples/todoApp/) dir.

Then, run the dev database:

```sh
./run wasp-cli db start
```

Leave it running, open a new terminal (tab), and run

```sh
./run wasp-cli db migrate-dev
```

to update the database schema.

Run

```sh
cp .env.server.example .env.server
```

to set up some mock env vars for the server, just to get the things running.

Finally, run

```sh
./run wasp-cli start
```

to run the example app in the development mode.

If you are doing this for the very first time, it might take a minute or so to download and install npm dependencies.

When done, new tab in your browser should open and you will see a Todo App!

> [!NOTE]
> You will notice that some functionality in the Todo App is not working. That is because the env vars in `.env.server` files are just mock values. Check Todo App's README for more details on how to set up env vars for development.

## Typical development workflow

1. Create a new feature branch from `main`.
2. If you don't have a good/reliable working HLS (Haskell Language Server) connected to your IDE (which we strongly recommend), you will want to instead run `./run ghcid` from the root of the `waspc` project: this will run a process that watches the Haskell project and reports any Haskell compiler errors. Leave it running.
   - NOTE: You will need to install `ghcid` globally first. You can do it with `cabal install ghcid`.
3. Do a change in the codebase (most often in `src/` or `cli/src/` or `data/`), and update tests if that makes sense (see [Test](#tests)).
   Fix any errors shown by HLS/`ghcid`.
   Rinse and repeat. If you're an internal team member, postpone updating waspc e2e tests tests until approval (see [here](#note-for-team-members)).
4. Use `./run build` to build the Haskell/cabal project, and `./run wasp-cli` to both build and run it. If you changed code in `packages/`, you will also need to run `./run build:packages` (check [TypeScript Packages section](#typescript-packages) for more details). Alternatively, you can also run slower `./run build:all` to at the same time build Haskell, TS packages, and any other piece of the project in one command.
5. For easier manual testing of the changes you did on a Wasp app, you have the `examples/todoApp` app, which we always keep updated. Also, if you added a new feature, add it to this app (+ tests) if needed. Check its README for more details (including how to run it).
6. Run `./run test` to confirm that all the tests are passing. If needed, accept changes in the waspc e2e tests with `./run test:waspc:e2e:accept-all`. Check "Tests" for more info.
7. If you did a bug fix, added new feature or did a breaking change, add short info about it to `Changelog.md`. Also, bump version in `waspc.cabal` and `ChangeLog.md` if needed. If you are not sure how to decide which version to go with, check out [how we determine the next version](#determining-next-version).
8. Create a PR. Keep an eye on CI tests -> Everything must pass. If it doesn't, look into it.
9. If your PR changes how users(Waspers) use Wasp, make sure to also update the documentation, which is in this same repo, but under `/web/docs`.
10. Work with reviewer(s) to get the PR approved, keep committing until the PR is approved.
11. Reviewer will merge the branch into `main`. Yay!

### Note for team members

Do not update waspc e2e tests until your PR is approved.

Accepting waspc e2e tests prematurely:

- Slows down the UI.
- Creates noise for the reviewers.
- Makes it less likely that you'll thoroughly check the final diffs after all review iterations.

**Carefully review waspc e2e tests diffs before accepting them** (see [Waspc e2e tests](#waspc-e2e-tests) for more details).

## Design docs (aka RFCs)

If the feature you are implementing is complex, be it due to its design or technical implementation, we recommend creating a [design doc](https://www.industrialempathy.com/posts/design-docs-at-google/) (aka RFC).
It is a great way to share the idea you have with others while also getting help and feedback.

To create one, make a PR that adds a markdown document under `wasp/docs/design-docs`, and in that markdown document explain the thinking behind and choices made when deciding how to implement a feature.

Others will comment on your design doc, and once it has gone through needed iterations and is approved, you can start with the implementation of the feature (in a separate PR).

## Codebase overview

Wasp is implemented in Haskell.

Codebase is split into library (`src/`) and CLI (which itself has a library `cli/src/` and thin executable wrapper `cli/exe/`).
CLI is actually `wasp` executable, and it uses the library, where most of the logic is.

Wasp compiler takes .wasp files + everything in the `src/` dir (JS, HTML, ...) and generates a web app that consists of a client, server, and database.

Wasp compiler code is split into 2 basic layers: Analyzer (frontend) and Generator (backend).

Wasp file(s) are analyzed by Analyzer, where they are first parsed, then typechecked, and then evaluated into a central IR (Intermediate Representation), which is `AppSpec` (`src/Wasp/AppSpec.hs`).
Check `src/Wasp/Analyzer.hs` for more details.

AppSpec is passed to the Generator, which based on it decides how to generate a web app.
Output of Generator is a list of FileDrafts, where each FileDraft explains how to create a file on the disk.
Therefore, Generator doesn't generate anything itself, instead it provides instructions (FileDrafts) on how to generate the web app.
FileDrafts are using mustache templates a lot (they can be found in `data/Generator/templates`).

Generator is split into three generators, for the three main parts of the web app: WebAppGenerator, ServerGenerator and DbGenerator.

After Generator provides us with FileDrafts, web app is generated by writing the FileDrafts to the disk.

![Waspc implementation diagram](images/waspc-implementation-diagram.png)

Generated web app consists of a client, server and database.

Client is written with React and react-query.
Server is written in NodeJS and uses ExpressJs.
Database is abstracted via Prisma.

We can run Wasp project with `wasp start`.
This will first compile the app, generate JS code in the `.wasp/out/` dir, and then run `npm start` for the client, `npm start` for the server, and also run the database.
On any changes you do to the source code of Wasp, Wasp project gets recompiled, and then changes in the generated code are picked up by the `npm start` of the client/server, therefore updating the web app.

## Important directories (in waspc/)

- `src/` -> main source code, library
- `cli/src/` -> rest of the source code, cli, uses library
- `cli/exe/` -> thin executable wrapper around cli library code
- `tests/`, `e2e-tests/`, `cli/tests/`, `waspls/tests/`, `starters-e2e-tests` -> tests
- `data/Generator/templates/` -> mustache templates for the generated client/server.
- `data/Cli/starters/` -> starter templates for new projects
- `examples/todoApp/` -> our kitchen sink app

### Typescript packages

`waspc`, while implemented in Haskell, relies on TypeScript for some of its functionality (e.g. for parsing TS code, or for deployment scripts). In these cases, the Haskell code runs these TS packages as separate processes and communicates through input/output streams. These packages are located in `packages/` and are normal npm projects. See `packages/README.md` for how the projects are expected to be set up.

In order for `waspc`'s Haskell code to correctly use these TS packages (and to also have them correctly bundled when generating the release tarball), they need to be correctly installed/built in the `waspc_datadir` dir.
To do so in development, run `./run build:packages` when any changes are made to these packages. We also run it in CI when building the release.

## Tests

For tests in Haskell we are using [**Tasty**](https://github.com/UnkindPartition/tasty) testing framework. Tasty lets us combine different types of tests into a single test suite.

In Tasty, there is a main test file that is run when test suite is run. In that file we need to manually compose test tree out of tests that we wrote. We organize tests in test groups, which are then recursively grouped resulting in a test tree.
Cool thing is that we can organize tests this way however we want and also mix different type of tests (hspec, quickcheck, and whatever else we want).

Tests are normally split in files of course, so we need to import those all the way up to the main test file, however we organize our test groups/trees.

In order to avoid need for manual organization and importing of test files described above, we are using [tasty-discover](https://hackage.haskell.org/package/tasty-discover) which does this for us.
It automatically detects files containing tests and organizes them for us into a test tree (and also takes care of importing).
This means we only need to create a file, write tests in it and that is it.
Test functions however do need to be prefixed with special prefix to indicate which type of test are they: spec* for Hspec, prop* for QuickCheck and similar.
Check docs for more details.
We can however still organize tests manually if we want in Tasty test trees, and then we just prefix them with test\_ and tasty-discover will pick them up from there.

Additionally, currently we limited tasty-discover to auto-detect only files ending with Test.hs (\*Test.hs glob).
We might remove that requirement in the future if it proves to have no benefit.

To summarize: If you are writing new tests, just put them in a file that ends with `Test.hs` in `tests/` dir and that is it.

For unit testing, we use **Hspec**.

For property testing, we use **Quickcheck**.

We additionally use **doctest** for testing code examples in documentation.

All tests go into `tests/` directory.
Haskell build tools don't have a good support for mixing them with source files.

To run tests:

- To run all tests, you can do `./run test`
- To run `waspc` tests only, you can do `./run test:waspc`.
- To run `waspc` unit tests only, you can do `./run test:waspc:unit`.
- To run individual unit test, you can do `./run test:waspc:unit "Some test description to match"`.
- To run `waspc` e2e tests only, you can do `./run test:waspc:e2e`.
- To run Wasp CLI tests only, you can do `./run test:cli`.
- To run Wasp LS tests only, you can do `./run test:waspls`.
- To run `todoApp` e2e tests, you can do `./run test:todoApp`.
- To run examples e2e tests, you can do `./run test:examples`.
- To run starter templates e2e tests, you can do `./run test:starters`.

### Waspc e2e tests

Inside of `waspc` e2e tests we have snapshot tests that run the `wasp-cli` on a couple of prepared projects, check that they successfully run, and also compare generated code with the expected generated code (golden output).

This means that when you make a change in your code that modifies the generated code, snapshot tests will fail while showing a diff between the new generated code and the previous (golden) one.
This gives you an opportunity to observe these differences and ensure that they are intentional and that you are satisfied with them.
**It is the PR author's (or the reviewers for outside contributions) responsibility to carefully review these diffs.**
Do not blindly accept changes, ensure they align with your intended modifications.
If you notice something unexpected or weird, you have an opportunity to fix it.
Once you are indeed happy with the changes in the generated code, you will want to update the golden output to the new (current) output, so that tests pass.
Basically, you want to say "I am ok with the changes and I accept them as the new state of things.".
Easiest way to do this is to use the convenient command from the `./run` script:

```sh
./run test:waspc:e2e:accept-all
```

## Code analysis

To run the Haskell code analysis, run:

```sh
./run code-check
```

This will check if code is correctly formatted, if it satisfies linter, and if it passes static analysis.

These same checks are required to pass the CI, so make sure this is passing before making a PR.

TODO: For now we check only the code formatting during the CI. In the future, once we make sure all the warnings are passing,
we will also check linter and static analysis during the CI, but that is not happening yet.

### Formatting

For formatting Haskell code we use [Ormolu](https://github.com/tweag/ormolu).

Normally we set it up in our editors to run on file save.

You can also run it manually with

```sh
./run ormolu:check
```

to see if there is any formatting that needs to be fixed, or with

```sh
./run ormolu:format
```

to have Ormolu actually format (in-place) all files that need formatting.

NOTE: When you run it for the first time it might take a while (~10 minutes) for all the dependencies to get installed.
The subsequent runs will be much faster.

### Linting

We use [hlint](https://github.com/ndmitchell/hlint) for linting our Haskell code.

You can use

```sh
./run hlint
```

to run the hlint on Wasp codebase.

NOTE: When you run it for the first time it might take a while (~10 minutes) for all the dependencies to get installed.
The subsequent runs will be much faster.

### Static Analysis

We use [stan](https://github.com/kowainik/stan) to statically analyze our codebase.

The easiest way to run it is to use

```sh
./run stan
```

This will build the codebase, run stan on it (while installing it first, if needed, with the correct version of GHC) and then write results to the CLI and also generate report in the `stan.html`.

NOTE: When you run it for the first time it might take a while (~10 minutes) for all the dependencies to get installed.
The subsequent runs will be much faster.

## Branching and merging strategy

This repo contains both the source code that makes up a Wasp release (under `waspc`), as well as our website containing documentation and blog posts (under `web`), and also Mage web app (under `mage`). In order to facilitate the development of Wasp code while still allowing for website / Mage updates or hotfixes of the current release, we have decided on the following minimal branching strategy.

All Wasp development should be done on feature branches. They form the basis of PRs that will target one of the two following branches:

- `main`: this branch contains all the actively developed new features and corresponding documentation updates. Some of these things may not yet be released, but anything merged into `main` should be in a release-ready state.
  - This is the default branch to target for any Wasp feature branches.
- `release`: this branch contains the source code of current/latest Wasp release, as well as the documentation and blog posts currently published and therefore visible on the website, and also currently published version of Mage.
  - When doing a full release, which means making a new release based on what we have currently on `main`, we do the following:
    1. Update `main` branch by merging `release` into it. There might be conflicts but they shouldn't be too hard to fix. Once `main` is updated, you can create a new waspc release from it, as well as deploy the website from it.
    2. Update `release` branch to this new `main` by merging `main` into it. There will be no conflicts since we already resolved all of them in the previous step.

How do I know where I want to target my PR, to `release` or `main`?

- If you have a change that you want to publish right now or very soon, certainly earlier than waiting till `main` is ready for publishing, then you want to target `release`. This could be website content update, new blog post, documentation (hot)fix, compiler hotfix that we need to release quickly via a new patch version, update for Mage that needs to go out now, ... .
- If you have a change that is not urgent and can wait until the next "normal" Wasp release is published, then target `main`. These are new features, refactorings, docs accompanying new features, ... .
- Stuff published on `release` (docs, Mage) uses/references version of `wasp` that was last released (so one that is also on `release`).
- TLDR;
  - `release` represents the present, and is for changes to the already published stuff.
  - `main` represents near future, and is for changes to the to-be-published stuff.

## Deployment / CI

We use Github Actions for CI.

CI runs for any commits on `main` branch, for pull requests, and for any commits tagged with tag that starts with `v`.

During CI, we build and test Wasp code on Linux, MacOS and Windows.

If commit is tagged with tag starting with `v`, github draft release is created from it containing binary packages.

We also have a workflow for deploying example apps to Fly.io (`deploy-examples.yml`). This workflow can be run manually from the GitHub UI and should typically be run from the `release` branch after publishing a new release to ensure the deployed examples are using the latest stable version of Wasp.

You can run this workflow:

- **From GitHub UI**: https://github.com/wasp-lang/wasp/actions/workflows/deploy-examples.yml (click "Run workflow" and select the `release` branch)
- **From GitHub CLI**:

  ```bash
  # Deploy with latest Wasp version
  gh workflow run deploy-examples.yml --ref release
  
  # Deploy with specific Wasp version
  gh workflow run deploy-examples.yml --ref release -f version=0.13.2
  ```

If you put `[skip ci]` in commit message, that commit will be ignored by Github Actions.

We also wrote a `new-release` script which you can use to help you with creating new release: you need to provide it with new version (`./new-release 0.3.0`) and it will check that everything is all right, create appropriate tag and push it, therefore triggering CI to create new release on Github.

NOTE: If building of your commit is suddenly taking much longer time, it might be connected with cache on Github Actions.
If it happens just once every so it is probably nothing to worry about. If it happens consistently, we should look into it.

### Typical Release Process

Do the steps marked with 👉 for every release of `waspc`.
Do the non-bold steps when necessary (decide for each step depending on the changes, e.g. some can be skipped if there were no breaking changes).

- Update the starter templates if necessary (i.e., if there are breaking changes or new features they should make use of):
  - Context: they are used by used by `wasp new`, you can find reference to them in `Wasp.Cli. ... .StarterTemplates`.
  - Check and merge all PRs with the label `merge-before-release`.
  - In `StarterTemplates.hs` file, update git tag to new version of Wasp we are about to release (e.g. `wasp-v0.13.1-template`).
  - Ensure that all starter templates are working with this new version of Wasp.
    Update Wasp version in their main.wasp files, and update their code as neccessary. Finally, in their repos (for those templates that are on Github), create new git tag that is the same as the new one in `StarterTemplates.hs` (e.g. `wasp-v0.13.1-template`). Now, once new wasp release is out, it will immediately be able to pull the correct and working version of the starter templates, which is why all this needs to happen before we release new wasp version.
  - Open-saas also falls under this!
- Make sure apps in [examples](/examples) are up to date and using a version compatible with the newest version of Wasp.
- Make sure that Wasp AI (which is part of `waspc` and you can run it with e.g. `wasp new:ai`) is correctly producing apps that work with and use this newest version of Wasp.
  This usually means checking that templates and prompts (e.g. examples of Wasp code) are up to date. If there were no breaking changes, there is likely nothing to be done here.
- Make sure [mage](/mage) is producing Wasp apps that support the newest version of Wasp (this is configured in its Dockerfile: which version of `wasp` CLI will it use to produce app via AI).
  Mage itself, as a Wasp app, doesn't have to be using the latest Wasp version to run, but the apps it is producing should.
- 👉 `ChangeLog.md` and the version in `waspc.cabal` should already be up to date, but double check that they are correct and update them if needed. Also consider enriching and polishing `ChangeLog.md` a bit even if all the data is already there. Also make sure the `ChangeLog.md` specifies the correct version of Wasp.
  - If you modify `ChangeLog.md` or `waspc.cabal`, create a PR, wait for approval and all the checks (CI) to pass, then squash and merge mentioned PR into `main`.
- 👉 Update your local repository state to have all remote changes (`git fetch`).\*\*
- 👉 Update `main` to contain changes from `release` by running `git merge release` while on the `main` branch. Resolve any conflicts.
- Take a versioned "snapshot" of the current docs by running `npm run docusaurus docs:version {version}` in the [web](/web) dir. Check the README in the `web` dir for more details. Commit this change to `main`.
- 👉 Fast-forward `release` to this new, updated `main` by running `git merge main` while on the `release` branch.
- 👉 Make sure you are on `release` and then run `./new-release 0.x.y`.
  - This will do some checks, tag it with new release version, and push it.
- 👉 Wait for CI to finish & succeed for the new tag.
  - This will automatically create a new draft release.
- 👉 Find new draft release here: https://github.com/wasp-lang/wasp/releases and edit it with your release notes.
- 👉 Publish the draft release when ready.
- 👉 Merge `release` back into `main` (`git merge release` while on the `main` branch), if needed.
- Deploy the example apps to Fly.io by running the [deploy-examples workflow](/.github/workflows/deploy-examples.yml) (see "Deployment / CI" section for more details).
- If there are changes to the docs, [publish the new version](/web#deployment) from the `release` branch.
- If you published new docs, rerun the Algolia Crawler to update the search index. If you published a new version of the docs, the search won't work until you do this.
  - To do this, go to https://crawler.algolia.com/admin and click "Restart crawling" under the "wasp-lang" crawler.
- If there are changes to Mage, [publish the new version](/mage#deployment) from the `release` branch.
- If there are changes to the [Wasp VSCode extension](https://github.com/wasp-lang/vscode-wasp), publish the new version.
- Announce new release in Discord if it makes sense.

#### Determining next version

`waspc` follows typical SemVer versioning scheme, so `major.minor.patch`.
There is one slightly peculiar thing though: waspc, besides being a wasp compiler and CLI, also contains wasp language server (waspls) inside it, under the subcommand `wasp waspls`.
So how do changes to waspls affect the version of waspc, since they are packaged together as one exe? We have decided, for practical reasons, to have them affect the patch number, possibly maybe minor, but not major.

#### Test releases (e.g. Release Candidate)

Making a test release, especially "Release Candidate" release is useful when you want to test the release without it being published to the normal users.
If doing this, steps are the following:

1. You can do it from whatever branch you want, probably you will be doing it from `main`.
2. You will want to use a version name that indicates you are doing test, probably you will want to add `-rc` at the end.
   So for example: `./new-release 0.7.0-rc`. Release script will throw some warnings which you should accept.
3. Once draft release is created on Github, you should mark it in their UI as pre-release and publish it. This will automatically remove the checkmark from "latest release", which is exactly what we want. This is the crucial step that differentiates test release from the proper release.
4. Since our wasp installer by default installs the latest release from Github, it will skip this release we made, because it is pre-release, which is great, it is what we wanted. Instead, you can install it by using the `-v` flag of wasp installer! That way user's don't get in touch with it, but we can install and use it normally.

## Documentation

External documentation, for users of Wasp, is hosted at https://wasp.sh/docs, and its source is available at [web/docs](/web/docs), next to the website and blog.

## Mage

Wasp's magic GPT web app generator aka Wasp AI aka Mage is hosted at https://usemage.ai and its source is available at [mage](/mage).

Make sure to update it when changes modify how Wasp works.

## Haskell

We are documenting best practices related to Haskell in our [Haskell Handbook](https://github.com/wasp-lang/haskell-handbook).

### Cabal dep freezing

Why don't we use a cabal freeze file to lock our dependencies?

In order to better support a wider range of developer operating systems, we have decided against using a cabal freeze file and instead use cabal's `index-state` feature to get package version pinning from hackage. See this question for more: https://github.com/haskell/cabal/issues/8059 .

## Code style guide

### General

#### Comments

##### Grammar

When writing a comment, we prefer starting it with a capital letter.

If it starts with a capital letter, it must end with a punctuation.

If it doesn't start with a capital letter, it shouldn't end with a punctuation.

##### TODO / NOTE

When writing a TODO or NOTE, use all capital letters, like this:

```hs
-- TODO: Wash the car.

-- NOTE: This piece of code is slow.
```

If you wish, you can add your name to TODO / NOTE. This is useful if you think there is a fair chance that reader of that TODO / NOTE might want to consult with its author.
You can do it like this:

```hs
-- TODO(martin): Doesn't work on my machine in some unusual use cases.
```

### JavaScript

#### Functions

For detailed reasoning/discussion on all listed rules, check [Issue #487](https://github.com/wasp-lang/wasp/issues/487).

##### Top-level named functions

When defining top-level named functions, we prefer to use statements:

```javascript
// good
function foo(param) {
  // ...
}

// bad
const foo = (param) => {
  // ...
};

// bad
const foo = function (param) {
  // ...
};
```

##### Inline function expression

When defining inline function expressions, we prefer the arrow syntax:

```javascript
// good
const squares = arr.map((x) => x * x);

// bad
const squares = arr.map(function (x) {
  return x * x;
});
```
