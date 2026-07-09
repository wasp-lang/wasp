# FSM Skateboard Context

## Scope

- Full-stack modules default-export a function from `module.wasp.ts`.
- The module function accepts `options` and returns a Wasp `Spec`.
- `examples/module` is the current skateboard module package: `@kitchen-sink/module`.
- Kitchen Sink default-imports the module function from `@kitchen-sink/module/spec`, calls it with `{ prefix: "/fsm" }`, and exposes the module route at `/fsm`.

## Module SDK Shim

- `wasp module install` and `wasp module build` copy the static shim into `.wasp/wasp` inside module projects.
- Shim files live in `data/Generator/templates/sdk/wasp/module-shim/`. Nothing in the shim is generated per module.
- Modules type server code with generic `Query` / `Action` types (`<Args, Result, Context>`). The real SDK's `wasp/server/operations` exports the same generics, so module operation code is valid host app code.
- `ambient.d.ts` declares the wildcard `declare module "wasp/*";`, so every other `wasp/*` import typechecks as `any`. Real declaration files (e.g. `wasp/server/operations`) win over the wildcard. The module tsconfig must include `.wasp/wasp/ambient.d.ts`.
- The shim is for module typechecking. Runtime `wasp/*` imports are provided by the host app SDK.
- An operation named `query` or `action` would generate a `Query`/`Action` SDK type colliding with the generics. Wasp does not validate this yet.

## Module Builder

- Spec build entry is `module.wasp.ts` and bundles dependencies into `dist/spec.js`.
- Module builder rejects `module.wasp.ts` files without a direct default export.
- The default client-route option is `prefix`.
- `dist/spec.d.ts` is rewritten to loose `any` exports to avoid duplicate branded `@wasp.sh/spec` types.
- Source build entries are discovered from `src/**/*.ts` and `src/**/*.tsx`, excluding `.d.ts`.
- Source entry names preserve package subpaths, e.g. `src/queries.ts` -> `dist/queries.js` -> `@pkg/queries`.
- Source build externalizes `react`, `react/jsx-runtime`, and all `wasp/*` imports.

## Host App Integration

- Host SDK Vite config dedupes the generated SDK package name (`wasp`) with React, React DOM, React Query, and React Router.
- Host SDK TypeScript config maps `wasp/*` to SDK source files during SDK build. This prevents self-imports from resolving through package exports into `dist/`, which otherwise causes TS5055 overwrite-input errors on repeated builds.
- Kitchen Sink snapshot setup copies `examples/module` as sibling `module`, builds it, then runs Kitchen Sink install/compile.
- Do not manually edit e2e snapshots. Regenerate with `./run test:waspc:e2e:accept-all` from `waspc/`.

## Local Module Dependencies

- Installing a module with `file:../module` creates an npm symlink to the module source directory.
- npm does not install linked local package dependencies into the consuming app. The linked module must run its own `npm install`.
- Direct host imports like `import("@kitchen-sink/module/actions")` can resolve the module's own dependencies from `examples/module/node_modules`.
- Generated server bundling follows symlinks (`preserveSymlinks: false`) and externalizes `node_modules`. This inlines linked module code into `.wasp/out/server/bundle/server.js` while leaving module dependencies as bare imports such as `quote-lib`.
- Once the import is in the generated server bundle, Node resolves it from the generated server package / host app, not from `examples/module/node_modules`, so symlinked local installs can fail at runtime with `ERR_MODULE_NOT_FOUND` for module dependencies.
- Packing the module with `npm pack` and installing the tarball avoids the symlink issue. npm installs the module dependency graph into the host app, so generated server bare imports like `quote-lib` resolve from the host `node_modules`.
- Kitchen Sink currently depends on the repo-local packed artifact at `file:../module/kitchen-sink-module-0.0.1.tgz` to model published-package behavior without custom registry setup.
- When module source changes, rebuild the module, run `npm run pack` from `examples/module/`, then refresh the Kitchen Sink lockfile (see next bullet).
- `package-lock.json` pins the tarball's `integrity` hash, and npm does not notice a changed tarball at the same version and path. With a warm npm cache it silently installs the stale cached content; with a cold cache (CI, e2e) it fails with `EINTEGRITY`. To refresh: replace the module's `integrity` value in `package-lock.json` with the new tarball hash (`sha512-$(openssl dgst -sha512 -binary <tarball> | base64)`), delete `node_modules/@kitchen-sink/module`, then run `wasp install`. Do not run plain `npm install` in a Wasp project dir. It prunes the Wasp-managed SDK entries (`.wasp/out/sdk/wasp`) from the lockfile.
- Do not commit absolute temp tarball paths. A real workflow should either publish/install a package artifact or make Wasp's local module development path preserve module dependency resolution.

## Known Limitations

- Only `wasp/server/operations` is typed in the shim. All other `wasp/*` imports are `any` via the ambient wildcard.
- `file:../module` local development does not faithfully model published package dependency resolution once the host server bundle follows symlinks into the module source.
- Module host app contracts are structural and local to the module. There is no generated type that proves the host app actually provides the expected Prisma model shape.

## Verification Commands

- `./run build`
- `npm run test` from `data/packages/module-builder/`
- `../../waspc/run wasp-cli module install && npm run typecheck && ../../waspc/run wasp-cli module build` from `examples/module/`
- `../../waspc/run wasp-cli install && ../../waspc/run wasp-cli compile` from `examples/kitchen-sink/`
- `./run test:waspc:e2e:accept-all`
