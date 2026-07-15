# Full-stack modules skateboard: decisions

Full-stack modules are npm packages that contribute declarations to a host app through `module.wasp.ts`. ✅ decision, ❌ rejected alternative.

## Spec

- ✅ Generated modules publish `dist/spec.js` as the runtime `./spec` export. `@wasp.sh/spec` remains external so the host supplies the runtime instance.
  - ❌ Publish `module.wasp.ts` for transformation by the host.
- ✅ Direct default export `(options) => Spec`, called explicitly by the host app.
  - ❌ Named exports, static specs, and default re-exports.
- ✅ `prefix` is the conventional client-route option. Wasp does not interpret it or namespace declarations and operation URLs.
- ✅ Relative refs carry an explicit logical origin. Package origins contain the canonical package name and package-relative spec path; project origins contain a project-relative spec path.
- ✅ Relative refs must stay inside `src/`. They map lexically to project `@src/<subpath>` or package `<packageName>/<subpath>` imports; file existence and package exports are not validated after transformation.
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
- ✅ Modules and apps validate `tsconfig.wasp.json` against the same required baseline. Module source validation reuses the app source baseline, replacing app project-reference output with `noEmit`, adding the SDK shim declaration, and requiring `jsx: react-jsx` instead of the app's `preserve`: module `dist/` ships plain `.js` that host bundlers never transform, so JSX must be compiled away at module build time. Module declaration emit uses the module config; host compilation uses the host config.
  - ❌ Hardcoded builder options or configs that violate the Wasp baseline.
- ✅ `wasp module build` typechecks the original and transformed module spec. The host typechecks only project-owned spec sources and the module's declaration boundary.
- ✅ Built spec artifacts are `dist/spec.js` and a bundled `dist/spec.d.ts`. The declaration bundle is self-contained except for package types such as the host's `@wasp.sh/spec`.
  - ❌ Use `module.wasp.ts` as the package's `types` target.

## Pipeline

- ✅ The app and module pipelines reuse compiler plugins from `@wasp.sh/spec/compiler`, supplying project or package logical origins per transformed spec file.
- ✅ Package `/spec` imports resolve to ordinary external JavaScript modules. The host has no package-source `.wasp.ts` resolver.
- ✅ Module spec imports of `@wasp.sh/spec` stay external and resolve to the host's copy.
  - ❌ Bundle a second spec-package instance.
- ✅ Generators render package-source external imports as package specifiers. Project-source imports continue through generated external-code paths.
- ✅ SDK compilation maps `wasp/*` to SDK source; host Vite dedupes the generated `wasp` package.

## Dependencies via Wasp CLI

- ✅ Generated packages use local `file:.wasp/...` dev dependencies for `@wasp.sh/spec` and the `wasp` shim. `wasp module install` refreshes both and runs `npm install` without rewriting `package.json`.
- ✅ `wasp module build` validates both module tsconfigs, refreshes the shim, and requires the installed `node_modules/@wasp.sh/spec` version to exactly match the CLI version.
- ✅ Generated modules declare required `@wasp.sh/spec` and React peers plus an optional `wasp` peer. The builder does not validate these fields.

## Packaging

- ✅ Published modules contain compiled `dist/` artifacts, package metadata, and documentation. They do not contain `module.wasp.ts` or `src/`. Runtime libraries are dependencies; host-provided libraries are peers.
- ✅ Kitchen Sink uses a checked-in tarball so the module's dependencies install into the host.
  - ❌ Default symlinked `file:../module`: the server bundle inlines module code but cannot resolve its bare dependency imports.
- ✅ Refresh the fixture by building and packing, updating lockfile `integrity`, removing the installed module, and running `wasp install`.

## Styling

- ✅ Modules ship component styles as plain CSS files side-effect-imported from source (`import "./X.css"`). The builder keeps relative CSS imports verbatim in compiled JavaScript and copies imported CSS files into `dist/` mirroring `src/` paths; host Vite code-splits them per chunk (verified in dev prebundling and prod build).
  - ❌ `@tsdown/css` single-bundle output: it strips CSS imports from emitted JS, breaking self-styling components, and forces a tsdown >= 0.22 upgrade.
  - ❌ Runtime CSS-in-JS: ecosystem in decline (styled-components is in maintenance mode), singleton/dedupe hazards in the host.
- ✅ The module shim declares `declare module "*.css";` so CSS side-effect imports typecheck. CSS Modules (typed class maps) are out of scope for the skateboard.
- ✅ CSS imports are client-only. A CSS import reachable from a module server entry fails the generated server bundle (Rollup parse error), since FSM packages are bundled into server.js.
- ✅ Module CSS is global in the host cascade. Convention: namespace class names with a module prefix and wrap the stylesheet in a named `@layer` so unlayered host CSS wins.
- ✅ Tailwind-styled modules compile in the host's Tailwind pass: the host opts in with `@source "../node_modules/<pkg>/dist"` (verified: the oxide scanner extracts static class strings from compiled dist). Modules must not precompile their own Tailwind CSS (duplicate utilities/preflight).
- ✅ Only CSS files actually imported by module source ship in dist; unimported CSS files are ignored.
- ✅ CSS imports are only supported from TypeScript sources. A CSS import inside a `src/**/*.js` file compiles into its importer's chunk, where the verbatim relative specifier no longer resolves.

## CLI

- ✅ `wasp module new <name>`, `wasp module install`, and `wasp module build` operate relative to the current directory.
- ✅ `wasp module new` flattens the complete package name into a lowercase directory name, replacing non-alphanumeric characters with `-`; for example, `@acme/fsm` becomes `acme-fsm/`.
- ✅ No watch mode in the skateboard. A future watcher belongs in Haskell and reruns the one-shot module build.

## Accepted for now

- ✅ The builder enforces direct `export default` syntax, but not that the exported value is an `(options) => Spec` function.
- ✅ The builder validates only a non-empty package name, not package metadata or name syntax.
- ✅ Every `src/**/*.ts` and `src/**/*.tsx` file becomes a public entry. JavaScript is not built, although ref mapping strips `.js` and `.jsx`.
- ✅ `tsdown` bundles module spec declarations after `wasp module build` performs module spec typechecking. `npm run typecheck` remains available for both module tsconfigs.
- ✅ npm package aliases are unsupported; generated imports use the module's canonical `package.json` name.
- ✅ Operations named `query` or `action` collide with the generic SDK aliases.
