# Packages

TypeScript packages of the Wasp monorepo:

- `wasp/` -> `@wasp/wasp`, the package Wasp projects depend on. It pins `@wasp/cli` and `@wasp/spec` to the same version. Published to npm.
- `cli/` -> `@wasp/cli`, the `wasp` executable wrapper. `scripts/make-npm-packages` turns it into the published package with the platform-specific binaries as optional dependencies.
- `spec/` -> `@wasp/spec`, the Wasp Spec users import in `main.wasp.ts`. Comes with `@wasp/wasp`. Published to npm.
- `deploy/`, `studio/` -> packages `waspc` runs as separate processes (see `Wasp.NodePackageFFI`).
- `libs/` -> Wasp libs, internal npm packages installed into generated apps. See [`libs/README.md`](libs/README.md).
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.

Together with `scripts/make-npm-packages` and `waspc/tools`, these form the repo's pnpm workspace
(`pnpm-workspace.yaml` at the repo root). Run `pnpm install` at the repo root once; the build scripts
below do it for you.

## Packages `waspc` ships in its data dir

`waspc` expects `spec`, `deploy` and `studio` under `waspc/data/packages/` and the libs
under `waspc/data/Generator/libs/`. Those directories are build outputs (gitignored):

- `./run build:packages` builds each package and copies `dist/` and `package.json` into
  `waspc/data/packages/<name>/`. `deploy` and `studio` get a `package-lock.json` generated
  there too, since `waspc` installs their dependencies with npm on first use. Their versions
  are therefore resolved at build time, not committed (TODO: goes away with
  [#4434](https://github.com/wasp-lang/wasp/issues/4434), when they ship with the Wasp npm package).
- `./run build:libs` builds each lib and packs it into `waspc/data/Generator/libs/<name>/`.
- `./run build:dev-packages` builds and packs `spec`, `cli` and `wasp` into `waspc/.dev-packages/`,
  for the Wasp projects in this repo to depend on the Wasp in this repo.

Then use `./run wasp-cli` as normal, or `./run install` and then `wasp-cli`.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`, add it to `pnpm-workspace.yaml` and to `WASPC_PACKAGES` in
`waspc/tools/packages/utils.ts` (as `runnable` if `waspc` runs it from the data dir, which
gets it a generated npm lockfile there), and add its files to `data-files` in `waspc.cabal`:

```
packages/<package-name>/package.json
packages/<package-name>/dist/**/*.js
```

Adjust the last line if the package emits other file extensions.

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
