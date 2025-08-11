# Testing Libs Locally

Run `tools/install_libs_to_data_dir.sh` to compile the libs and copy
them into `data/`. Then you can use `cabal run` as normal, or you can
`cabal install` and then use `wasp-cli`.

# Adding a New Package

Create a directory in this folder to contain the new package. The package will be packaged
using `npm pack` and the resulting tarball will be copied to `data/libs/` by the
`tools/install_libs_to_data_dir.sh` script.

# CI Builds/Release

The CI workflow runs the package install script, and `tools/make_binary_package.sh`
takes care of copying data files into the release archive.
