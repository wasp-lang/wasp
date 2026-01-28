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

## Lib Version

We don't version the libraries by semantic versioning like usual npm packages.
Instead, we use a fixed version `0.0.0` in the `package.json` of each lib.
They are considered to be an implementation detail of the Wasp CLI, so their version
is whatever version the Wasp CLI is.

When the Wasp CLI is shipped, the libs are packaged and shipped with it in the
`waspc/data/` folder, and the Wasp CLI uses these local copies of the libs
when generating the Wasp app.

### `npm` cache busting

`npm` caches installed packages based on their version, so if we used a fixed version
like `0.0.0` for the libs, when a user updates their Wasp CLI `npm` would use the
cached version of the lib instead of the new version that came with the updated Wasp CLI.

To avoid this, the Wasp CLI computes a checksum of the tarball of the lib and uses
that as the version when generating the Wasp app. This way, if a lib changes,
the checksum changes, and `npm` will install the new version of the lib.

```json
{
  "dependencies": {
    "@wasp/lib-name": "file:./libs/lib-name-<checksum>.tgz"
  }
}
```

## Lib Exports Naming Convention

Libs can be used in different contexts:

- Server,
- Web app,
- Wasp SDK.

So each piece of code can be for different runtimes:

- Node.js,
- Browser,
- Neutral (can be used in both runtimes).

Here's the list of possible exports you can have in a lib:

| Export Path | Node.js | Browser | Description                  |
| ----------- | ------- | ------- | ---------------------------- |
| `./index`   | ✅      | ✅      | Code used in both runtimes   |
| `./node`    | ✅      | ❌      | Code used in Node.js runtime |
| `./browser` | ❌      | ✅      | Code used in browser runtime |

For example, if you have an `auth` lib that has code that's for both runtimes, browser only code and Node.js only code you would have the following exports in `auth/package.json`:

```json
"exports": {
  "./index": {
    "types": "./dist/index.d.ts",
    "default": "./dist/index.js"
  },
  "./node": {
    "types": "./dist/node.d.ts",
    "default": "./dist/node.js"
  },
  "./browser": {
    "types": "./dist/browser.d.ts",
    "default": "./dist/browser.js"
  }
},
```

Which would expose the following import paths:

- `@wasp.sh/lib-auth` (for both runtimes),
- `@wasp.sh/lib-auth/node` (for Node.js only code),
- `@wasp.sh/lib-auth/browser` (for browser-only code).

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
