# Phase 8: Scaffolding & Watch Mode

`wasp module init` scaffolding templates and `--watch` mode for dev workflow.

## Goal

Module authors can quickly bootstrap a new module project and develop with hot-reload.

## `wasp module init [name]`

### Generated Structure

```
@myorg/my-module/
├── module.wasp.ts
├── src/
│   └── index.ts
├── package.json
└── tsconfig.json
```

### `module.wasp.ts` Template

```typescript
import { createMyModule } from "./src/index.js";

export default createMyModule({});
```

### `src/index.ts` Template

```typescript
import { Module } from "wasp-config";

export type MyModuleConfig = {};

export function createMyModule(config: MyModuleConfig): Module {
  const mod = new Module("@myorg/my-module");

  // mod.entity("Example", {
  //   fields: {
  //     id: "Int @id @default(autoincrement())",
  //   },
  // });

  return mod;
}
```

### `package.json` Template

```jsonc
{
  "name": "@myorg/my-module",
  "version": "0.1.0",
  "type": "module",
  "main": "dist/src/index.js",
  "types": "dist/src/index.d.ts",
  "exports": {
    ".": { "types": "./dist/src/index.d.ts", "default": "./dist/src/index.js" },
    "./*": { "types": "./dist/src/*.d.ts", "default": "./dist/src/*.js" }
  },
  "files": ["dist"],
  "scripts": {
    "build": "wasp module build && tsc",
    "dev": "wasp module build --watch",
    "test": "vitest"
  },
  "peerDependencies": {
    "wasp-config": "^0.x"
  },
  "devDependencies": {
    "wasp-config": "file:.wasp/packages/wasp-config",
    "typescript": "^5.0.0"
  }
}
```

### `tsconfig.json` Template

Standard TypeScript config with module resolution pointing at the generated SDK.

## Watch Mode (`wasp module build --watch`)

### Watched Paths

- `module.wasp.ts`
- `src/**/*.ts`, `src/**/*.tsx`

### Behavior

1. Run the full build pipeline (Phase 5)
2. Watch for file changes via `fsnotify` (or similar)
3. On change, re-run the pipeline
4. Report errors inline, continue watching

### Files to Create/Modify

| File | Change |
|---|---|
| New: scaffold templates | `module.wasp.ts`, `src/index.ts`, `package.json`, `tsconfig.json` |
| `Wasp/Cli/Command/Module.hs` | `init` subcommand reads templates and writes to disk |
| `Wasp/Cli/Command/Module.hs` | `build --watch` flag triggers file watcher loop |

## Publishing Workflow

After development:

```
$ wasp module build    # final type-check
$ npm publish
```

The `dist/` folder ships the factory function and implementations. It does NOT ship `module.wasp.ts` or `.wasp/`.

## Verification

- `wasp module init @test/example` creates all expected files
- Generated `module.wasp.ts` is valid and builds with `wasp module build`
- Watch mode detects file changes and re-runs the pipeline
- Watch mode reports errors inline and continues watching
