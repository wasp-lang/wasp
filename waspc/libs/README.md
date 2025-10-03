# Wasp Libs

Wasp Libs are Wasp-owned npm packages that contain code that will be used in the
generated Wasp apps. They are building blocks that are used in the generated Wasp
apps.

There are two ways you can add code to the generated Wasp apps:

- by writing Mustache templates in the `waspc/data/Generator/templates` folder
- by working on the libs in this folder

Templates are not a real JS project (they include Mustache syntax) which means you
can't write tests for them, and they can't be type-checked.
The libs, on the other hand, are real JS projects, and you can write tests for them,
and they are type-checked.

Ideally, most of the logic should be in the libs, and the templates should produce
config objects and orchestrate the use of these libs.

## Testing Libs Locally

Wasp Libs are npm libraries you develop in isolation and test them using unit tests.

When you want to test how they integrate with the Wasp CLI and the generated app,
you need to make sure the Wasp CLI can find them.
To do that, you need to copy the compiled libs to the `waspc/data/` folder, which is
packaged with the Wasp CLI.
Run `./run build:libs` to compile the libs and copy them into `data/`.
Then you can use `./run wasp-cli` as you normally would.

## Adding a New Lib

Create a directory in this folder to contain the new package.

Keep in mind:

- `package.json` should have a `prepare` script that will be run to prepare
  the package for use e.g. to build the package.
- `package.json` should include a `files` field that specifies which files
  should be included e.g. `"files": ["dist"]` if the built files are in `dist/`.

The package will be packaged using `npm pack` and the resulting tarball will
be copied to `waspc/data/libs/` by the `./run build:libs` script.

Make sure to add this new library to the `Wasp.Generator.WaspLibs.AvailableLibs`
module so that the Wasp CLI knows about it.
