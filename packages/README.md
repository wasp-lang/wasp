# Packages

TypeScript packages of the Wasp monorepo:

- `cli/` -> `@wasp/cli`, the `wasp` executable wrapper. `scripts/make-npm-packages` turns it into the published package with the platform-specific binaries as optional dependencies.
- `spec/` -> `@wasp/spec`, the package users import in `main.wasp.ts`. Published to npm.
- `deploy/`, `studio/` -> packages `waspc` runs as separate processes (see `Wasp.NodePackageFFI`).
- `libs/` -> Wasp libs, internal npm packages installed into generated apps. See [`libs/README.md`](libs/README.md).
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.

Together with `scripts/make-npm-packages` and `waspc/tools`, these form the repo's pnpm workspace
(`pnpm-workspace.yaml` at the repo root). Run `pnpm install` at the repo root once; the build scripts
below do it for you.

## Packages `waspc` ships in its data dir

`waspc` expects `spec`, `deploy` and `studio` under `waspc/data/packages/` and the libs
under `waspc/data/Generator/libs/`. Those directories are build outputs (gitignored):

- Each package's `build` mise task (e.g. `mise run //packages/spec:build`) builds it and copies
  `dist/` and `package.json` into `waspc/data/packages/<name>/`. `deploy` and `studio` get a `package-lock.json` generated
  there too, since `waspc` installs their dependencies with npm on first use. Their versions
  are therefore resolved at build time, not committed (TODO: goes away with
  [#4434](https://github.com/wasp-lang/wasp/issues/4434), when they ship with the Wasp npm package).
- Each lib's `build` mise task (e.g. `mise run //packages/libs/auth:build`) builds it and packs
  it into `waspc/data/Generator/libs/<name>/`.

`mise run //waspc:build` runs all of these first, in dependency order, and skips the ones whose
sources haven't changed. So do the waspc e2e tests. Then use `wasp-cli` (which mise puts on
`PATH` inside the repo) as normal.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`, add it to `pnpm-workspace.yaml` and to `WASPC_PACKAGES` in
`waspc/tools/packages/utils.ts` (as `runnable` if `waspc` runs it from the data dir, which
gets it a generated npm lockfile there). Give it a `mise.toml` with a `build` task that extends
the `wasp:package:build` template, like the other packages, and add it to the `wasp:waspc`
project's `depends` in the root `mise.toml`. Then add its files to `data-files` in `waspc.cabal`:

```
packages/<package-name>/package.json
packages/<package-name>/dist/**/*.js
```

Adjust the last line if the package emits other file extensions.

## CI builds and release

CI runs the same build tasks, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
