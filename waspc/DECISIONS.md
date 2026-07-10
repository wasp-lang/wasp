# Full-stack modules skateboard: decisions

Full-stack modules are npm packages that contribute declarations to a host app through `module.wasp.ts`. ✅ decision, ❌ rejected alternative.

## Spec

- ✅ Generated modules publish `module.wasp.ts` as the runtime `./spec` export for the Wasp pipeline. Direct Node execution is unsupported. The host applies its normal spec transforms and typechecking.
  - ❌ Build `dist/spec.js`: duplicates ref lowering and bundles the module's incompatible `@wasp.sh/spec` types.
- ✅ Direct default export `(options) => Spec`, called explicitly by the host app.
  - ❌ Named exports, static specs, and default re-exports.
- ✅ `prefix` is the conventional client-route option. Wasp does not interpret it or namespace declarations and operation URLs.
- ✅ Relative refs in package specs must stay inside the package's `src/`. They map lexically to `<packageName>/<subpath>`; file existence and package exports are not validated.
- ✅ Absolute ref paths are rejected. Non-relative refs are package specifiers. `@src/foo` has no legacy project-source meaning and is interpreted as the package name `@src/foo`.
  - ❌ Full npm package-specifier validation; only incomplete scoped names such as `@scope` are rejected.
- ✅ AppSpec external imports carry a tagged source: a project `@src/...` path or a package name plus optional subpath. The legacy Wasp DSL continues to produce project-source imports.

## Types

- ✅ A static, type-only `wasp` shim supplies generic `Query` and `Action` types (`<Args, Result, Context>`). An ambient `declare module "wasp/*"` fallback covers other SDK paths.
  - ❌ Generate per-operation shim types by parsing or evaluating the spec.
  - ❌ Mirror the complete SDK declaration surface or use `export = any`.
- ✅ The host SDK exports the same unconstrained `Query` and `Action` aliases. They do not encode serialization, auth, entities, or generated-wrapper compatibility.
  - ❌ Shim-only generics or incompatible per-operation generics.
- ✅ Module operations declare their required structural context. The demo leaves its Prisma delegate as `any` because the host model shape cannot be verified.
  - ❌ Mock Prisma delegate types.
- ✅ Modules and apps validate `tsconfig.wasp.json` against the same required baseline. Module source validation reuses the app source baseline, replacing app project-reference output with `noEmit` and adding the SDK shim declaration. Module declaration emit uses the module config; host compilation uses the host config.
  - ❌ Hardcoded builder options or configs that violate the Wasp baseline.
- ✅ After bundling, the host typechecks all transformed `*.wasp.ts` sources, including imported module specs. Error diagnostics fail compilation; non-error diagnostics warn. This checks bundled spec sources, not every source file in the module.
  - ❌ Skip package specs or downgrade errors to warnings.
- ✅ The only built spec artifact is `dist/spec.d.ts`, emitted with `noCheck` from the module config. Its references resolve against the host's `@wasp.sh/spec`.
  - ❌ Use `module.wasp.ts` as the package's `types` target.

## Pipeline

- ✅ The spec pipeline replaces unrun's external resolver, bundling bare imports that resolve to `*.wasp.ts`. Externalized package specs would execute as raw TypeScript without ref lowering.
  - ❌ `resolveId`, wrapping or vendoring unrun, or replacing unrun with rolldown in this skateboard.
- ✅ Module spec imports of `@wasp.sh/spec` stay external and resolve to the host's copy.
  - ❌ Bundle a second spec-package instance.
- ✅ Generators render package-source external imports as package specifiers. Project-source imports continue through generated external-code paths.
- ✅ SDK compilation maps `wasp/*` to SDK source; host Vite dedupes the generated `wasp` package.

## Dependencies via Wasp CLI

- ✅ Generated packages use local `file:.wasp/...` dev dependencies for `@wasp.sh/spec` and the `wasp` shim. `wasp module install` refreshes both and runs `npm install` without rewriting `package.json`.
- ✅ `wasp module build` validates both module tsconfigs, refreshes the shim, and requires the installed `node_modules/@wasp.sh/spec` version to exactly match the CLI version.
- ✅ Generated modules declare required `@wasp.sh/spec` and React peers plus an optional `wasp` peer. The builder does not validate these fields.

## Packaging

- ✅ Published modules are npm artifacts containing `module.wasp.ts`, `src/`, and compiled `dist/`. Runtime libraries are dependencies; host-provided libraries are peers.
- ✅ Kitchen Sink uses a checked-in tarball so the module's dependencies install into the host.
  - ❌ Default symlinked `file:../module`: the server bundle inlines module code but cannot resolve its bare dependency imports.
- ✅ Refresh the fixture by building and packing, updating lockfile `integrity`, removing the installed module, and running `wasp install`.

## CLI

- ✅ `wasp module new <name>`, `wasp module install`, and `wasp module build` operate relative to the current directory.
- ✅ `wasp module new` flattens the complete package name into a lowercase directory name, replacing non-alphanumeric characters with `-`; for example, `@acme/fsm` becomes `acme-fsm/`.
- ✅ No watch mode in the skateboard. A future watcher belongs in Haskell and reruns the one-shot module build.

## Accepted for now

- ✅ The builder enforces direct `export default` syntax, but not that the exported value is an `(options) => Spec` function.
- ✅ The builder validates only a non-empty package name, not package metadata or name syntax.
- ✅ Every `src/**/*.ts` and `src/**/*.tsx` file becomes a public entry. JavaScript is not built, although ref mapping strips `.js` and `.jsx`.
- ✅ Declaration emit uses `noCheck`; `npm run typecheck` is separate.
- ✅ Operations named `query` or `action` collide with the generic SDK aliases.
