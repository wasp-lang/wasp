# Packages

TypeScript packages of the Wasp monorepo:

- `spec/` -> `@wasp/spec`, the package users import in `main.wasp.ts`. Published to npm.
- `deploy/`, `prisma/`, `studio/` -> packages `waspc` runs as separate processes (see `Wasp.NodePackageFFI`).
- `libs/` -> Wasp libs, internal npm packages installed into generated apps. See [`libs/README.md`](libs/README.md).
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.

Together with `scripts/make-npm-packages` and `waspc/tools`, these form the repo's pnpm workspace
(`pnpm-workspace.yaml` at the repo root). `deploy`, `prisma` and `studio` are the exception: they
stay standalone npm packages with their own `package-lock.json`, because `waspc` ships that lockfile
and installs them with npm on first use. Run `pnpm install` at the repo root once; the build scripts
below do it for you.

## Packages `waspc` ships in its data dir

`waspc` expects `spec`, `deploy`, `prisma` and `studio` under `waspc/data/packages/` and the libs
under `waspc/data/Generator/libs/`. Those directories are build outputs (gitignored):

- `./run build:packages` builds each package and copies `dist/`, `package.json` and, for the
  npm packages, `package-lock.json` into `waspc/data/packages/<name>/`.
- `./run build:libs` builds each lib and packs it into `waspc/data/Generator/libs/<name>/`.

Then use `./run wasp-cli` as normal, or `./run install` and then `wasp-cli`.

### Adding a new package that `waspc` runs

Create a directory in this folder with a `build` script in `package.json` that compiles to
`dist/`, add it to `pnpm-workspace.yaml` and to `WASPC_PACKAGES` in
`waspc/tools/packages/utils.ts`, and add its files to `data-files` in `waspc.cabal`:

```
packages/<package-name>/package.json
packages/<package-name>/dist/**/*.js
```

Adjust the last line if the package emits other file extensions.

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
