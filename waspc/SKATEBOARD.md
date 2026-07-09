# FSM Skateboard Context

## Scope

- Full-stack modules default-export a function from `module.wasp.ts` that accepts `options` and returns a Wasp `Spec`. Direct `export default` only.
- The default client-route option is `prefix`.
- Invariant: spec files (`*.wasp.ts`) are a Wasp dialect. Only the Wasp CLI spec pipeline transforms them. Modules ship `module.wasp.ts` as source and never ship pre-transformed spec code.
- `examples/module` is the current skateboard module package: `@kitchen-sink/module`. Kitchen Sink imports it from `@kitchen-sink/module/spec`, calls it with `{ prefix: "/fsm" }`, and exposes the module route at `/fsm`.

## Module SDK Shim

- `wasp module install` and `wasp module build` copy the static shim from `data/Generator/templates/sdk/wasp/module-shim/` into `.wasp/wasp`. Nothing in the shim is generated per module.
- Modules type server code with generic `Query` / `Action` types (`<Args, Result, Context>`). The real SDK's `wasp/server/operations` exports the same generics, so module operation code is valid host app code.
- `ambient.d.ts` declares the wildcard `declare module "wasp/*";`, so every other `wasp/*` import typechecks as `any`. Real declaration files (e.g. `wasp/server/operations`) win over the wildcard. Both module tsconfigs must include `.wasp/wasp/ambient.d.ts`.
- The shim is for module typechecking. Runtime `wasp/*` imports are provided by the host app SDK.
- An operation named `query` or `action` would generate a `Query`/`Action` SDK type colliding with the generics. Wasp does not validate this yet.

## Module Builder

- The spec is not built. The package's `/spec` export points at `module.wasp.ts`. The builder only emits `dist/spec.d.ts` (declaration-only, `noCheck`) for editor support in the host app. Its type references resolve against the host's `@wasp.sh/spec`, so there are no duplicate branded types.
- Modules must have the same `tsconfig.wasp.json` apps have, so the spec file is compiled the same way in the module and in the host app. `wasp module install` and `wasp module build` validate it with the app validator (`Wasp.Project.ExternalConfig.WaspTsConfig`), and the `dist/spec.d.ts` emit reads its compiler options.
- Source code lives under `tsconfig.src.json`; `tsconfig.json` is an app-style solution file referencing both.
- Source build entries are discovered from `src/**/*.ts` and `src/**/*.tsx`, excluding `.d.ts`. Entry names preserve package subpaths, e.g. `src/queries.ts` -> `dist/queries.js` -> `@pkg/queries`.
- Source build externalizes `react`, `react/jsx-runtime`, and all `wasp/*` imports.
- Watch mode rebuilds only `src/` entries. `dist/spec.d.ts` is emitted once per build invocation.

## Host App Integration

- The pipeline's external resolver (`externalResolver.ts`) keeps bare imports external except those that resolve to `*.wasp.ts` files, which get bundled so the host's own plugins lower the module's ref imports. It must be the `external` option (not a `resolveId` plugin) because the bundler consults `external` before plugin resolution.
- `mapRefObject` maps relative refs from package-resident spec files to package import sources: the nearest `package.json` names the package, and `./src/<subpath>` maps to the `<packageName>/<subpath>` export backed by `dist/`.
- The host typechecks module spec files like its own. A broken module fails the host compile with a type error, not spec evaluation with a runtime error.
- The module's `@wasp.sh/spec` import stays external and resolves to the host's copy at spec evaluation time, so there is a single spec package instance.
- Host SDK Vite config dedupes the generated SDK package name (`wasp`) with React, React DOM, React Query, and React Router.
- Host SDK TypeScript config maps `wasp/*` to SDK source files during SDK build. This prevents self-imports from resolving through package exports into `dist/`, which otherwise causes TS5055 overwrite-input errors on repeated builds.
- Kitchen Sink snapshot setup copies `examples/module` as sibling `module`, builds it, then runs Kitchen Sink install/compile.

## Local Module Dependencies

- Symlinked installs (`file:../module`) break at runtime: the generated server bundle follows symlinks and inlines module code, leaving module dependencies as bare imports (e.g. `quote-lib`) that Node then resolves from the host `node_modules`, where npm never installed them (`ERR_MODULE_NOT_FOUND`).
- Packing the module (`npm pack`) and installing the tarball fixes this: npm installs the module's dependency graph into the host. Kitchen Sink depends on `file:../module/kitchen-sink-module-0.0.1.tgz` to model published-package behavior without a registry.
- npm treats `name@version` tarballs as immutable: `package-lock.json` pins the tarball's `integrity` hash and npm never re-reads changed bytes at the same version and path. A warm npm cache silently installs the stale cached content; a cold cache (CI, e2e) fails with `EINTEGRITY`.
- After changing module source, refresh Kitchen Sink like this (from `examples/module/`):

  ```sh
  ../../waspc/run wasp-cli module build
  npm run pack
  cd ../kitchen-sink
  node -e '
    const fs = require("fs");
    const { createHash } = require("crypto");
    const tarball = fs.readFileSync("../module/kitchen-sink-module-0.0.1.tgz");
    const lock = JSON.parse(fs.readFileSync("package-lock.json", "utf8"));
    lock.packages["node_modules/@kitchen-sink/module"].integrity =
      "sha512-" + createHash("sha512").update(tarball).digest("base64");
    fs.writeFileSync("package-lock.json", JSON.stringify(lock, null, 2) + "\n");
  '
  rm -rf node_modules/@kitchen-sink/module
  ../../waspc/run wasp-cli install
  ```

- Do not run plain `npm install` in a Wasp project dir. It prunes the Wasp-managed SDK entries (`.wasp/out/sdk/wasp`) from the lockfile.
- Future work: `wasp install` should detect `file:` tarball dependencies whose on-disk hash diverges from the lockfile and refresh the entry itself. Same tarball cache-busting problem as https://github.com/wasp-lang/wasp/issues/4357.

## Known Limitations

- Only `wasp/server/operations` is typed in the shim. All other `wasp/*` imports are `any` via the ambient wildcard.
- Module host app contracts are structural and local to the module. There is no generated type that proves the host app actually provides the expected Prisma model shape.

## Verification Commands

- `./run build`
- `npm run test` from `data/packages/module-builder/`
- `../../waspc/run wasp-cli module install && npm run typecheck && ../../waspc/run wasp-cli module build` from `examples/module/`
- `../../waspc/run wasp-cli install && ../../waspc/run wasp-cli compile` from `examples/kitchen-sink/`
- `./run test:waspc:e2e:accept-all`
