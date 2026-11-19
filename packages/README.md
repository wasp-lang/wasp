# Packages

TypeScript packages of the Wasp monorepo:

- `wasp/` -> `@wasp/wasp`, the package Wasp projects depend on. It pins every other package here, except `wasp-app-runner`, to its own version, which is the Wasp compiler's version (e.g. `0.27.0`). Published to npm.
- `cli/` -> `@wasp/cli`, the `wasp` executable wrapper. `cli/scripts/make-cli-npm-packages` turns it into the published package with the platform-specific binaries as optional dependencies.
- `spec/` -> `@wasp/spec`, the package that exports the Wasp Spec, which users import in `main.wasp.ts`. Published to npm.
- `deploy/`, `studio/` -> `@wasp/deploy` and `@wasp/studio`, the implementations of `wasp deploy` and `wasp studio`. Published to npm.
- `wasp-app-runner/` -> CLI for running Wasp apps in e2e tests. Published to npm.
- The rest -> the code that the generated app imports (see [below](#packages-the-generated-code-imports)). Published to npm.

Together with `packages/cli/scripts/make-cli-npm-packages` and `waspc/tools`, these form the repo's pnpm workspace
(`pnpm-workspace.yaml` at the repo root). Run `pnpm install` at the repo root once; the build scripts
below do it for you.

## Build outputs

Wasp projects in this repo depend on the gitignored dev tarballs under `waspc/.dev-packages/`.
`mise run build:dev-packages` builds and packs `wasp` and every package it depends on into it.

Then use `mise run wasp-cli` as normal, or `mise run install` and then `wasp-cli`. A globally
installed `wasp-cli` needs `WASP_DEV_PACKAGES_DIR=<repo>/waspc/.dev-packages` only for `wasp-cli new`,
so that the new project depends on the dev packages. `mise run install` prints the exact command.

After you change a package, a Wasp app in this repo has to install it again. Run
`mise run build:dev-packages`, then from the root of the app:

```bash
mise run reinstall-dev-wasp-app-deps
```

This command forces `npm` to reinstall the dev tarballs.

### Adding a new package

Create a directory in this folder, named like the package without the `@wasp/` scope. Its
`package.json` needs a `build` script that compiles to `dist/`. `mise run build:dev-packages`
and `mise run test:packages` run it.

Then add the package to:

- `pnpm-workspace.yaml`
- the `dependencies` of `wasp/package.json`. The repo's TS tooling reads the package list from
  there (see `waspc/tools/packages/utils.ts`).
- the `overrides` of the example apps

If `waspc` runs the package, give `@wasp/wasp` a run script for it in `wasp/scripts/` (see
`run-deploy.js`), export it from `wasp/package.json`, and add it to `WaspNpmPackageScript` in
`Wasp.Project.WaspNpmPackage`.

### Changing the peer dependencies of `@wasp/wasp`

The `peerDependencies` of `wasp/package.json` are the packages the generated code needs in every
app (React, Express, Prisma and so on). No other place lists them: `waspc` reads them from the
project's installed `@wasp/wasp`. When you change one:

- `@prisma/client` and `prisma` must have the same version. Also update
  `waspc/data/Generator/templates/sdk/wasp/prisma-runtime-library.d.ts`.
- `react` and `react-dom` must have the same range. Also update `react` in the packages that
  list it (e.g. `auth/package.json`).
- `@types/react` and `@types/react-dom` follow React's major version. Also update `@types/react`
  in the packages that list it.
- `@types/express` and `@types/express-serve-static-core` must have the same range.

## Packages the generated code imports

There are two ways to add code to the generated Wasp apps:

- by writing Mustache templates in the `waspc/data/Generator/templates` folder
- by working on the packages in this folder that the generated code imports, e.g. `sdk-core/`

Templates are not a real JS project, since they include Mustache syntax. You can't write tests
for them or type-check them. The packages are real JS projects, with tests and type checks.

Ideally, most of the logic should be in the packages, and the templates should produce config
objects and orchestrate the use of the packages.

### Exports naming convention

The generated server, web app and Wasp SDK all use these packages, so each piece of code is for
Node.js, for the browser, or for both.

| Export Path | Node.js | Browser | Description                  |
| ----------- | ------- | ------- | ---------------------------- |
| `.`         | ✅      | ✅      | Code used in both runtimes   |
| `./node`    | ✅      | ❌      | Code used in Node.js runtime |
| `./browser` | ❌      | ✅      | Code used in browser runtime |

## CI builds and release

CI runs the same build scripts, and `waspc/tools/make_binary_package.sh` copies the data files
into the release archive. `scripts/pack-published-packages.sh` packs the packages that get
published to npm with the same version as the Wasp CLI.
