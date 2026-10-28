# Packages

TypeScript packages of the Wasp monorepo:

- `spec/` -> `@wasp/spec`, the package users import in `main.wasp.ts`. Published to npm.
- `deploy/`, `prisma/`, `studio/` -> packages `waspc` runs as separate processes (see `Wasp.NodePackageFFI`).
- `libs/` -> Wasp libs, internal npm packages installed into generated apps. See [`libs/README.md`](libs/README.md).
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.

## Packages `waspc` ships in its data dir

`waspc` expects `spec`, `deploy`, `prisma` and `studio` under `waspc/data/packages/` and the libs
under `waspc/data/Generator/libs/`. Those directories are build outputs (gitignored):

- `./run build:packages` builds each package and copies `dist/`, `package.json` and
  `package-lock.json` into `waspc/data/packages/<name>/`.
- `./run build:libs` builds each lib and packs it into `waspc/data/Generator/libs/<name>/`.

Then use `./run wasp-cli` as normal, or `./run install` and then `wasp-cli`.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`. Add its name to `WASPC_PACKAGE_NAMES` in `waspc/tools/packages/utils.ts`, and add its
files to `data-files` in `waspc.cabal`:

```
packages/<package-name>/package.json
packages/<package-name>/package-lock.json
packages/<package-name>/dist/**/*.js
```

Adjust the last line if the package emits other file extensions.

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
