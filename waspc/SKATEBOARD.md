# FSM Skateboard Context

## Scope

- Full-stack modules default-export a function from `module.wasp.ts` that accepts `options` and returns a Wasp `Spec`. Direct `export default` only.
- The default client-route option is `prefix`.
- Invariant: spec files (`*.wasp.ts`) are a Wasp dialect. `wasp module build` compiles module specs to JavaScript before publication; host apps never consume package-resident Wasp source.
- `examples/module` is the current skateboard module package: `@kitchen-sink/module`. Kitchen Sink imports it from `@kitchen-sink/module/spec`, calls it with `{ prefix: "/fsm" }`, and exposes the module route at `/fsm`.
- The demo module exercises all 8 module-usable declaration kinds: route, page, query, action, crud, api, apiNamespace, and job.

## Module SDK Shim

- `wasp module install` and `wasp module build` copy the static shim from `data/Generator/templates/sdk/wasp/module-shim/` into `.wasp/wasp`. Nothing in the shim is generated per module.
- Modules type server code with generic `Query` / `Action` types (`<Args, Result, Context>`). The real SDK's `wasp/server/operations` exports the same generics, so module operation code is valid host app code.
- `ambient.d.ts` declares the wildcard `declare module "wasp/*";`, so every other `wasp/*` import typechecks as `any`. Real declaration files (e.g. `wasp/server/operations`) win over the wildcard. Both module tsconfigs must include `.wasp/wasp/ambient.d.ts`.
- The shim is for module typechecking. Runtime `wasp/*` imports are provided by the host app SDK.
- An operation named `query` or `action` would generate a `Query`/`Action` SDK type colliding with the generics. Wasp does not validate this yet.

## Module Builder

- The package's `/spec` export points at `dist/spec.js`, with `dist/spec.d.ts` for editor support. The compiled JavaScript keeps `@wasp.sh/spec` external and resolves to the host's peer dependency at evaluation time.
- The app and module pipelines share the ref-lowering and transformed-source typechecking plugins from `@wasp.sh/spec/compiler`.
- Ref helpers carry logical origins. Project origins contain a project-relative spec path; package origins contain the package name and package-relative spec path. Relative refs are mapped with pure POSIX path arithmetic, without absolute paths or package filesystem discovery.
- Modules use the same `tsconfig.wasp.json` baseline as apps. `wasp module build` typechecks the original module spec, typechecks its transformed source, and bundles its public declarations into `dist/spec.d.ts`.
- Source code lives under `tsconfig.src.json`; `tsconfig.json` is an app-style solution file referencing both.
- Source build entries are discovered from `src/**/*.ts` and `src/**/*.tsx`, excluding `.d.ts`. Entry names preserve package subpaths, e.g. `src/queries.ts` -> `dist/queries.js` -> `@pkg/queries`.
- Source build externalizes `react`, `react/jsx-runtime`, and all `wasp/*` imports.
- Relative CSS imports (`import "./X.css"`) pass through the source build: the specifier stays verbatim in the compiled JavaScript and the imported CSS file is copied into `dist/` mirroring its `src/` path. The host bundler handles the rest.
- Imported CSS files must exist and resolve inside `src/`; the build fails otherwise. Bare/package CSS imports remain a hard build error. Unimported CSS files are not copied.
- The demo module ships `MainPage.css` this way, imported from `MainPage.tsx`.
- There is no module build watch mode.

## Host App Integration

- Module `/spec` exports are ordinary external ESM dependencies. The host pipeline transforms and typechecks only project-owned `*.wasp.ts` files.
- `mapRefObject` maps package logical origins and relative descriptors such as `./src/<subpath>` to `<packageName>/<subpath>` imports backed by `dist/`.
- Module source errors fail `wasp module build`. The host typechecks the module factory call through `dist/spec.d.ts` and performs host-specific entity and declaration validation after evaluation.
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
- Module refs are static exports, so referenced behavior (e.g. middleware) cannot be parameterized by `options`. Only plain-data declaration values, such as the apiNamespace path, can derive from `options`.
- Module host app contracts are structural and local to the module. There is no generated type that proves the host app actually provides the expected Prisma model shape.
- npm aliases for module packages are not supported because compiled logical origins carry the canonical `package.json` name used in generated imports.
- Module CSS imports are client-only. A CSS import reachable from a module server entry fails the generated server bundle (Rollup parse error), since FSM packages are bundled into server.js.
- Module CSS lands in the host's global cascade. Convention: namespace class names with a module prefix and wrap the stylesheet in a named `@layer` so unlayered host CSS wins.
- Modules must not precompile their own Tailwind CSS. Tailwind-styled modules compile in the host's Tailwind pass; the host opts in with `@source "../node_modules/<pkg>/dist"`.
- No CSS Modules. The shim's `declare module "*.css";` types all CSS imports as `any`; there is no class-map typing.

## Verification Commands

- `./run build`
- `npm run test` from `data/packages/module-builder/`
- `../../waspc/run wasp-cli module install && npm run typecheck && ../../waspc/run wasp-cli module build` from `examples/module/`
- `../../waspc/run wasp-cli install && ../../waspc/run wasp-cli compile` from `examples/kitchen-sink/`
- `./run test:waspc:e2e:accept-all`
