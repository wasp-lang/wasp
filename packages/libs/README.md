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

We version the libraries as the current Wasp compiler version e.g. `0.19.2`.
They are considered to be an implementation detail of Wasp, so their version
is whatever version Wasp is.

We publish the libs to npm with each Wasp release. `@wasp/wasp` depends on them at its own
version, so a project gets them when it installs `@wasp/wasp`, and the generated code imports
them from the project's `node_modules`.

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
| `.`         | ✅      | ✅      | Code used in both runtimes   |
| `./node`    | ✅      | ❌      | Code used in Node.js runtime |
| `./browser` | ❌      | ✅      | Code used in browser runtime |

For example, if you have an `auth` lib that has code that's for both runtimes, browser only code and Node.js only code you would have the following exports in `auth/package.json`:

```json
"exports": {
  ".": {
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

- `@wasp/lib-auth` (for both runtimes),
- `@wasp/lib-auth/node` (for Node.js only code),
- `@wasp/lib-auth/browser` (for browser-only code).

## Testing Libs Locally

Wasp Libs are npm libraries you develop in isolation and test them using unit tests.

When you want to test how they integrate with the generated app, the app has to install the
libs you built. The Wasp apps in this repo depend on dev tarballs of `@wasp/wasp` and the
packages it brings, the libs included.

Run `mise run build:dev-packages` to compile the libs and pack them into `waspc/.dev-packages/`.
Then reinstall them from the root of the Wasp app:

```bash
mise run reinstall-dev-wasp-app-deps
```

This command removes the entries of the dev Wasp npm packages from `package-lock.json` and runs
`npm install`. `npm` would otherwise fail on the repacked tarballs, since their content changed
while their version didn't.

## Adding a New Lib

Create a directory in this folder to contain the new package.

Keep in mind:

- `package.json` should have a `build` script that compiles the package to
  `dist/`. `mise run build:dev-packages` and `mise run test:libs` run it, so a lib never
  has to build itself on install.
- `package.json` should include a `files` field that specifies which files
  should be included e.g. `"files": ["dist"]` if the built files are in `dist/`.

Then add the lib to the `dependencies` of `packages/wasp/package.json` and follow the rest
of [Adding a new package](../README.md#adding-a-new-package-that-waspc-runs) (the publish
script and the `overrides` of the example apps).
