# Phase 1: wasp-config TypeScript Foundation

Entity declarations in `Module.ts`, serialization to Haskell, and `run.ts` branching.

## Goal

`Module.entity()` stores field declarations. `run.ts` detects a `Module` default export and serializes entity declarations so the Haskell compiler can consume them.

## Files to Modify

| File | Change |
|---|---|
| `src/publicApi/Module.ts` | `entity()` stores field declarations (name + fields map) |
| `src/publicApi/tsAppSpec.ts` | Add `entityDeclarations` to module spec output type |
| `src/mapTsAppSpecToAppSpecDecls.ts` | Serialize entity declarations for Haskell consumption |
| `src/run.ts` | Branch on `Module` vs `App` default export |
| `src/appSpec.ts` | Add `EntityDeclaration` type to the IR |

## Details

### 1. `Module.entity(name, { fields })`

```typescript
// Module.ts
entity(name: string, opts: { fields: Record<string, string> }): void {
  this._entityDeclarations.push({
    name,
    fields: opts.fields,
  });
}
```

Store as `EntityDeclaration[]` on the `Module` instance:

```typescript
type EntityDeclaration = {
  name: string;
  fields: Record<string, string>; // fieldName → Prisma field type string
};
```

### 2. `tsAppSpec.ts` Extension

Add `entityDeclarations` to the module portion of the spec output. When a module is used via `app.use()`, its entity declarations travel alongside the existing entity map.

### 3. Serialization (`mapTsAppSpecToAppSpecDecls.ts`)

Entity declarations need to be serialized into the JSON that the Haskell compiler reads. Format:

```json
{
  "entityDeclarations": [
    {
      "name": "Todo",
      "fields": {
        "id": "Int @id @default(autoincrement())",
        "text": "String",
        "isDone": "Boolean @default(false)"
      }
    }
  ]
}
```

### 4. `run.ts` Branching

```typescript
const defaultExport = await loadWaspTs(waspTsSpecPath);

if (defaultExport instanceof Module) {
  const moduleSpec = extractModuleSpec(defaultExport);
  writeOutput(outputFilePath, { mode: "module", spec: moduleSpec });
} else if (defaultExport instanceof App) {
  const appSpec = extractAppSpec(defaultExport, entityNames);
  writeOutput(outputFilePath, { mode: "app", spec: appSpec });
} else {
  throw new Error("Default export must be an App or Module instance");
}
```

## Verification

- Unit tests: `mapTsAppSpecToAppSpecDecls.unit.test.ts` covers entity declaration serialization
- Manual: a `module.wasp.ts` with `mod.entity(...)` calls produces correct JSON output
