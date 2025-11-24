# Testing Packages Locally

Run `./run build:packages` to compile the packages and copy
them into `waspc/data/`. Then you can use `./run wasp-cli` as normal, or you can
run `./run install` and then use `wasp-cli`.

# Adding a New Package

Create a directory in this folder to contain the new package. It should have a
`build` script inside `package.json` as well as a `start` script that calls the
compiled code.

Then, in `data-files` inside `waspc.cabal`, add these files:

```
packages/<package-name>/package.json
packages/<package-name>/package-lock.json
packages/<package-name>/dist/**/*.js
```

The last line assumes the project is compiled to `.js` files inside the `dist`
directory. You should adjust this and/or add more file extensions if needed.

# CI Builds/Release

The CI workflow runs the package install script, and `tools/make_binary_package.sh`
takes care of copying data files into the release archive.
