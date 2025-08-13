# Auth Library

This library exports code related to Wasp's auth. It's a result of migrating the auth code from
Mustache templates to a proper npm package.

## What's inside

The library has multiple exports:

- `sdk` - used in the Wasp SDK in any runtime (`@wasp.sh/libs-auth/sdk`)
- `sdk/browser` - used in the Wasp SDK in the browser runtime (`@wasp.sh/libs-auth/sdk/browser`)
- `server` - used in the `server` app (`@wasp.sh/libs-auth/server`)

If you are writing browser-only code, make sure to put it in a `<name>/browser` export.
For example, if writing browser-only code for the `sdk` export, put it in `sdk/browser`.

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

When you need to add a new entry point e.g. `@wasp.sh/libs-auth/web-app`:

1. Add a new entry in `tsdown.config.ts` with the name and entry path.

   ```ts
   createNewEntry({
     name: "web-app", // Name of the file in the dist directory i.e. web-app.js
     entryPath: "./src/web-app/index.ts",
     platform: "browser",
   }),
   ```

2. Add the new entry to `typesVersions` in `package.json`.

   ```json
   "typesVersions": {
     "*": {
       "web-app": [
         "./dist/web-app.d.ts"
       ]
     }
   }
   ```

3. Add the new entry to `exports` in `package.json`.

   ```json
   "exports": {
     "./web-app": {
       "types": "./dist/web-app.d.ts",
       "import": "./dist/web-app.js"
     }
   }
   ```

### Coverage & type checking

To check the code coverage and typecheck the code, run:

```bash
npm run test
```

## Building the library

To build the library, run:

```bash
npm run build
```

We are using `tsdown` to build the library which you can configure in `tsdown.config.ts`.

### Verifying type exports

To check the type exports are okay and the library can be consumed, run:

```bash
npx @arethetypeswrong/cli -P
```

You want to see `🟢` for all items and `⚠️ ESM (dynamic import only) ` for `node16`.
