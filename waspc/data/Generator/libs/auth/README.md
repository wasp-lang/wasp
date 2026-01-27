# Auth Library

This library exports code related to Wasp's auth which is used in the generated Wasp apps' code.

## What's inside

The has multiple entry points:

- `sdk` - used in the Wasp SDK in any runtime (`@wasp.sh/lib-auth/sdk`)
- `sdk/browser` - used in the Wasp SDK in the browser runtime (`@wasp.sh/lib-auth/sdk/browser`)
- `server` - used in the `server` app (`@wasp.sh/lib-auth/server`)

Read more about the naming convention for exports in the parent [README](../README.md#lib-exports-naming-convention).

### Lib version

Read more about versioning and why we pin the version to `0.0.0` in the parent [README](../README.md#lib-version).

## Development

Install the dependencies:

```bash
npm install
```

Start unit tests in watch mode:

```bash
npm run dev
```

Develop code and write unit tests for it.

### Adding a new export

When you need to add a new entry point e.g. `@wasp.sh/lib-auth/web-app`:

1. Add a new entry in `tsdown.config.ts` with the name and entry path.

   ```ts
   createNewEntry({
     name: "web-app", // Name of the file in the dist directory i.e. web-app.js
     entryPath: "./src/web-app/index.ts",
     platform: "browser",
   }),
   ```

2. Add the new entry to `exports` in `package.json`.

   ```json
   "exports": {
     "./web-app": {
       "types": "./dist/web-app.d.ts",
       "import": "./dist/web-app.js"
     }
   }
   ```

## Running checks

We have a few checks that we run to ensure the library is in good shape. You can run them all at once with:

```bash
npm run check
```

### Tests coverage

To check the test coverage, run:

```bash
npm run check:coverage
```

### Type checking

To type check the code, run:

```bash
npm run check:types
```

### Verifying type exports

This check uses [`arethetypeswrong`](https://github.com/arethetypeswrong/arethetypeswrong.github.io) under the hood which:

> attempts to analyze npm package contents for issues with their TypeScript types,
> particularly ESM-related module resolution issues

We use the `--profile esm-only` when running the check because we only care about the ESM related checks since this library will be only imported using ESM.

To check the type exports are okay run:

```bash
npm run check:type-exports
```

You want to see `🟢` for all ESM related check since this library will be only imported using ESM. This means the library's
types will resolve correctly when used in the SDK, the server and the web app.

## Building the library

To build the library, run:

```bash
npm run build
```

We are using `tsdown` to build the library which you can configure in `tsdown.config.ts`.
