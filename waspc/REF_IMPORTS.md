# Ref Imports Plan

## Goal

Let users write normal relative paths for references to app code while keeping the Haskell compiler and generated app assumptions stable.

User-facing API:

```ts
const LoginPage = refImport({ importDefault: "LoginPage", from: "./LoginPage" })
```

Internal compiler JSON stays unchanged:

```json
{ "kind": "default", "name": "LoginPage", "path": "@src/LoginPage" }
```

## Core Concepts

`RefImport` is the public API concept. Users produce it with `refImport(...)` or `with { type: "ref" }` imports.

`ExtImport` is the internal `AppSpec` JSON/Haskell concept. It must keep normalized `@src/...` paths.

`sourceFilePath` is explicit metadata attached by `makeRefImport(import.meta.url)`. It is visible on runtime `RefImport` objects and stripped when mapping to `AppSpec.ExtImport`.

`RefImport` has a visible `kind: "refImport"` marker from the start. A TypeScript-only brand is not enough because the mapper must eventually reject raw descriptor objects at runtime.

Path normalization happens once, at the AppSpec mapping boundary, in `mapRefImportToExtImport`.

Raw `@src` descriptor objects are not a compatibility requirement. They can exist temporarily only while tracer bullets are in progress, and every transitional source/test use must have a precise `// TODO:` removal comment.

## Target Flow

```text
*.wasp.ts source
  -> source transform
  -> refImport(...) runtime values with sourceFilePath
  -> mapRefImportToExtImport(projectRootDir)
  -> AppSpec.ExtImport JSON with @src/...
  -> Haskell unchanged
```

## Tracer Bullets

### 1. Introduce Public RefImport

Rename the public concept from `ExtImport` to `RefImport`.

Primary files:

- `data/packages/spec/src/spec/extImport.ts` to `data/packages/spec/src/spec/refImport.ts`
- `data/packages/spec/src/spec/publicApi/index.ts`
- `data/packages/spec/src/spec/publicApi/tsAppSpec.ts`

Add:

```ts
/**
 * User-facing reference to code in your app's `src` directory.
 */
export type RefImport<T extends RefImportDescriptor = RefImportDescriptor> = T & {
  kind: "refImport"
}

export type RefImportDescriptor =
  | NamedRefImportDescriptor
  | DefaultRefImportDescriptor

export function refImport<T extends RefImportDescriptor>(
  descriptor: T,
): RefImport<T> {
  return { ...descriptor, kind: "refImport" }
}
```

Temporarily keep raw `ExtImport` in `Reference<T>` because current lowering still emits plain descriptors.

Required TODO:

```ts
// TODO: Remove raw ExtImport from Reference after reference import lowering
// emits refImport(...) calls instead of plain descriptors.
```

### 2. Extract Path Normalization

Add shared path normalization:

- `data/packages/spec/src/spec/refImportPath.ts`

API:

```ts
normalizeRefImportPath({
  importPath,
  importingFilePath,
  projectRootDir,
})
```

Behavior:

- Resolves relative paths against the `.wasp.ts` file.
- Rejects paths outside app `src/`.
- Returns normalized `@src/...`.
- Owns generic error wording, with no caller-specific `importLabel`.

Current lowering can keep using this helper temporarily.

### 3. Add makeRefImport(import.meta.url)

Add source-aware helper:

```ts
export type RefImportSource = {
  sourceFilePath: string
}

export type RefImportWithSourcePath<
  T extends RefImportDescriptor = RefImportDescriptor,
> = RefImport<T & RefImportSource>

export function makeRefImport(importingFileUrl: string): SourceAwareRefImportHelper
```

`makeRefImport(import.meta.url)` attaches visible `sourceFilePath`.

Test one runtime behavior:

```ts
makeRefImport(fileUrl)({
  import: "archive",
  from: "./operations",
  alias: "archiveTask",
})
```

It returns the descriptor plus `kind: "refImport"` and `sourceFilePath`.

### 4. Reshape App Mapping

Rename and reshape mapping so `projectRootDir` enters once.

Rename:

- `mapApp.ts` to `mapAppToAppSpecDecls.ts`
- `mapApp` to `mapAppToAppSpecDecls`

Add:

```ts
export type AppSpecMappingContext = {
  entityNames: string[]
  projectRootDir: string
}

export function makeAppSpecMapper(context: AppSpecMappingContext)
```

Individual mapping functions become methods on the mapper object.

Avoid threading helpers like:

```ts
mapQuery(query, entityRefParser, mapRefImport)
```

Use closure state instead.

If a temporary pass-through seam is introduced, it must have a TODO:

```ts
// TODO: Replace this pass-through with mapRefImportToExtImport after RefImport
// path normalization moves to the AppSpec mapping boundary.
```

### 5. Move Normalization To Mapping Boundary

Add:

```ts
mapRefImportToExtImport(refImport, { projectRootDir })
getRefImportDeclarationName(refImport)
```

Delete `mapExtImport` and update all callers.

`mapRefImportToExtImport` handles:

- Named refs.
- Default refs.
- Relative path normalization using `sourceFilePath`.
- Missing `sourceFilePath` errors for relative paths.

Raw `@src` descriptors remain only temporarily if needed. That branch needs a TODO:

```ts
// TODO: Remove raw @src descriptor support after reference import lowering
// emits refImport(...) calls instead of plain descriptors.
```

### 6. Rewrite Explicit User refImport Imports

Source transform rewrites:

```ts
import { refImport, page } from "@wasp.sh/spec"
```

to:

```ts
import { page, makeRefImport } from "@wasp.sh/spec"
const refImport = makeRefImport(import.meta.url)
```

Support:

- `refImport as ref`
- type-only imports left unchanged
- mixed type/value imports preserving type specifiers
- existing `makeRefImport` import reuse

Prefer a dedicated transform:

```ts
ensureSourceAwareRefImport({
  sourceText,
  sourcePath,
  required,
})
```

This transform owns helper import rewriting and injection.

When `required` is true and the file does not import `refImport`, this transform injects a local `refImport` binding so reference import lowering can emit helper calls in the same file.

It returns the helper's local name so lowering can emit calls through the same binding. This avoids duplicate helpers when users write `import { refImport as ref } from "@wasp.sh/spec"`.

### 7. Lower with { type: "ref" } Into refImport(...)

Change lowering output from plain objects to helper calls.

Current:

```ts
const LoginPage = { importDefault: "LoginPage", from: "@src/LoginPage" } as const
```

Target:

```ts
const LoginPage = refImport({ importDefault: "LoginPage", from: "./LoginPage" })
```

Requirements:

- Preserve user-written paths.
- Remove `as const`.
- Stop normalizing paths in the lowering planner.
- Stop validating paths in the lowering planner.
- Validate all used refs in `mapRefImportToExtImport`.
- Use the local helper name returned by `ensureSourceAwareRefImport`.

Namespace target:

```ts
const ops = new Proxy({}, {
  get: (_t, k) => refImport({
    import: String(k),
    from: "./operations",
    alias: "ops_" + String(k),
  }),
}) as Record<string, ReturnType<typeof refImport>>
```

This bullet removes TODOs tied to old lowering emitting plain descriptors.

### 8. Remove Raw Public ExtImport

Remove raw descriptor compatibility after all generated and user-facing reference paths produce `RefImport` values.

Update:

```ts
export type Reference<T> = RefImport | T
```

Remove from public API:

- `ExtImport`
- `NamedExtImport`
- `DefaultExtImport`

Keep internal:

- `data/packages/spec/src/appSpec.ts` `AppSpec.ExtImport`
- `src/Wasp/AppSpec/ExtImport.hs`

Add permanent tests that raw descriptors are rejected at type and runtime boundaries.

## Naming Cleanup

Use names that match the domain boundary:

- Public source reference: `RefImport`
- Internal JSON/Haskell reference: `AppSpec.ExtImport`
- Source transform for import attributes: `lowerReferenceImports`
- Planner directory: `planReferenceImportLowering`
- Mapper file: `mapAppToAppSpecDecls.ts`
- Declaration-name helper: `getRefImportDeclarationName`

## TODO Rule

Every transitional source or test change must include a precise `// TODO:` with the removal condition.

Do not write comments like "temporary" or "first tracer bullet".

Good:

```ts
// TODO: Remove raw ExtImport from Reference after reference import lowering
// emits refImport(...) calls instead of plain descriptors.
```

Bad:

```ts
// Temporary during migration.
```

Permanent new behavior tests for `RefImport`, `makeRefImport`, and final lowering do not need TODOs.

## Verification

Run targeted package checks first:

```sh
npm run test:unit
npm run test:type
npm run test:integration
```

Then run package-level verification from `waspc`:

```sh
./run build:packages
./run test:packages
```

If e2e snapshots change, regenerate snapshots only through the accepted workflow:

```sh
./run build
./run test:waspc:e2e:accept-all
```

Do not manually edit e2e snapshots.
