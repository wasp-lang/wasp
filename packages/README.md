# Packages

TypeScript packages of the Wasp monorepo:

- `wasp/` -> `@wasp/wasp`, the package Wasp projects depend on. It pins `@wasp/cli`, `@wasp/spec`, `@wasp/deploy` and `@wasp/studio` to the same version. Published to npm.
- `cli/` -> `@wasp/cli`, the `wasp` executable wrapper. `scripts/make-npm-packages` turns it into the published package with the platform-specific binaries as optional dependencies.
- `spec/` -> `@wasp/spec`, the Wasp Spec users import in `main.wasp.ts`. Published to npm.
- `deploy/`, `studio/` -> `@wasp/deploy` and `@wasp/studio`, the implementations of `wasp deploy` and `wasp studio`. `waspc` runs them from the project's `@wasp/wasp` (see `Wasp.NodePackageFFI`). Published to npm.
- `libs/` -> Wasp libs, internal npm packages installed into generated apps. See [`libs/README.md`](libs/README.md).
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.

Together with `scripts/make-npm-packages` and `waspc/tools`, these form the repo's pnpm workspace
(`pnpm-workspace.yaml` at the repo root). Run `pnpm install` at the repo root once; the build scripts
below do it for you.

## Build outputs

`waspc` expects the libs under `waspc/data/Generator/libs/`, and Wasp projects in this repo
depend on the dev tarballs under `waspc/.dev-packages/`. Both are build outputs (gitignored):

- `./run build:libs` builds each lib and packs it into `waspc/data/Generator/libs/<name>/`.
- `./run build:dev-packages` builds and packs `spec`, `deploy`, `studio`, `cli` and `wasp`
  into `waspc/.dev-packages/`.

Then use `./run wasp-cli` as normal, or `./run install` and then `wasp-cli`. A globally
installed `wasp-cli` also needs `WASP_DEV_PACKAGES_DIR=<repo>/waspc/.dev-packages` exported,
so that `wasp-cli new` makes projects depend on the dev packages. `./run install` prints the exact line.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`. Then add the package to:

- `pnpm-workspace.yaml`
- `WASP_PACKAGE_DIRS` in `waspc/tools/packages/utils.ts`
- both lists in `scripts/pack-published-packages.sh`
- the `dependencies` of `wasp/package.json`
- the packages `waspc/tools/dev-packages/build.ts` builds and packs
- the dev tarball lists in `Wasp.Project.WaspNpmPackage`, `Wasp.Cli.Command.Build`,
  `waspc/e2e-tests/ShellCommands.hs`, `waspc/run` and `waspc/tools/version-bump.ts`
- the `overrides` of the example apps

Give `@wasp/wasp` a run script for the package (see `wasp/run-deploy.js`), add the script to
the `files` of `wasp/package.json`, and point `Wasp.Project.WaspNpmPackage` at it.

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
