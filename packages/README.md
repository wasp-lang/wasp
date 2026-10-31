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

Then use `./run wasp-cli` as normal, or `./run install` and then `wasp-cli`.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`, add it to `pnpm-workspace.yaml`, to `WASP_PACKAGE_NAMES` in
`waspc/tools/packages/utils.ts`, to the dependencies of `wasp/package.json` and to the
packages `waspc/tools/dev-packages/build.ts` builds and packs. Give `@wasp/wasp` a run script
for it (see `wasp/run-deploy.js`) and point `Wasp.Project.WaspNpmPackage` at that script.

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
