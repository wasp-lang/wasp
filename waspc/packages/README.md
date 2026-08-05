# Testing Packages Locally

These packages are npm workspaces of `waspc/`, so their dependencies all live in
a single `waspc/node_modules` and are locked by a single `waspc/package-lock.json`.
Install them with `npm install` from `waspc/`.

Run `./run build:packages` to compile the packages. Then you can use `./run wasp-cli`
as normal, or you can `./run install` and then use `wasp-cli`.

# Adding a New Package

Create a directory in this folder to contain the new package. It should have a
`build` script inside `package.json`, and its `version` must match the version in
`waspc.cabal`.

The packages aren't bundled into the Wasp binary anymore: they're published to
npm and installed as dependencies of the `wasp` npm package. So, for waspc to be
able to run the new package, you also have to:

1. Give it a `bin` entry named `__internal_wasp_<package-name>__`, which is the
   name waspc invokes (npm puts it on the `PATH`).
2. Add its package name to `RUNNABLE_PACKAGE_NAMES` in
   `scripts/make-npm-packages/src/common.ts`, so the `wasp` npm package depends
   on it.
3. Add it to `RunnablePackage` in `waspc/src/Wasp/NodePackageFFI.hs`, along with
   its bin name.

# CI Builds/Release

The CI workflow runs the package install script, publishes it to npm (in
release) or pkg.pr.new (in PRs), with the same version number as the Wasp CLI.
