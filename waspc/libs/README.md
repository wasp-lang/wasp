# Testing Libs Locally

Run `tools/install_libs_to_data_dir.sh` to compile the libs and copy
them into `data/`. Then you can use `./run wasp-cli` as normal. You can run
`./run install` which will run the script before installing the
Wasp CLI.

# Adding a New Package

Create a directory in this folder to contain the new package. It should contain a
`prepare` script that will be run to prepare the package for use e.g. to build
the package.

The package will be packaged using `npm pack` and the resulting tarball will
be copied to `data/libs/` by the `tools/install_libs_to_data_dir.sh` script.

# CI Builds/Release

The CI workflow runs the package install script, and `tools/make_binary_package.sh`
takes care of copying data files into the release archive.
