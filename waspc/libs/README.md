# Libs

Libs are npm packages that contain code used by the Wasp apps. They are building
blocks that are used to build the Wasp SDK, the `server` and the `web-app`.

There are two ways you can add code to the generated Wasp apps:

- by writing Mustache templates in the `data/Generator/templates` folder
- by working on the libs in this folder

Templates are not a real JS project (they include Mustache syntax) which means you
can't write tests for them, and they are not type-checked.
The libs, on the other hand, are real JS projects, and you can write tests for them,
and they are type-checked.

Ideally, most of the logic should be in the libs, and the templates should produce
config objects and orchestrate the use of these libs.

## Testing Libs Locally

Run `tools/install_libs_to_data_dir.sh` to compile the libs and copy
them into `data/`. Then you can use `./run wasp-cli` as normal. You can run
`./run install` which will run the script before installing the
Wasp CLI.

## Adding a New Lib

Create a directory in this folder to contain the new package. It should contain a
`prepare` script that will be run to prepare the package for use e.g. to build
the package.

The package will be packaged using `npm pack` and the resulting tarball will
be copied to `data/libs/` by the `tools/install_libs_to_data_dir.sh` script.

## CI Builds/Release

The CI workflow runs the package install script, and `tools/make_binary_package.sh`
takes care of copying data files into the release archive.
