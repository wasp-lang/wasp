# FSM Skateboard Context

## Scope

- Full-stack modules default-export a Wasp `Spec` from `module.wasp.ts`.
- `examples/module` is the current skateboard module package: `@kitchen-sink/module`.
- Kitchen Sink default-imports the module spec from `@kitchen-sink/module/spec` and exposes the module route at `/fsm`.

## Module SDK Shim

- `wasp module install` and `wasp module build` generate `.wasp/wasp` inside module projects.
- Shim templates live in `data/Generator/templates/sdk/wasp/module-shim/`.
- The shim package is named `wasp` and only types:
  - `wasp/client/operations`
  - `wasp/server/operations`
- The shim is for module typechecking. Runtime `wasp/*` imports are provided by the host app SDK.
- Operation shim exports are derived from `query(name)` / `action(name)` calls in `module.wasp.ts`.

## Module Builder

- Spec build entry is `module.wasp.ts` and bundles dependencies into `dist/spec.js`.
- Module builder rejects `module.wasp.ts` files without a default export.
- `dist/spec.d.ts` is rewritten to loose `any` exports to avoid duplicate branded `@wasp.sh/spec` types.
- Source build entries are discovered from `src/**/*.ts` and `src/**/*.tsx`, excluding `.d.ts`.
- Source entry names preserve package subpaths, e.g. `src/queries.ts` -> `dist/queries.js` -> `@pkg/queries`.
- Source build externalizes `react`, `react/jsx-runtime`, and all `wasp/*` imports.

## Host App Integration

- Host SDK Vite config dedupes the generated SDK package name (`wasp`) with React, React DOM, React Query, and React Router.
- Kitchen Sink snapshot setup copies `examples/module` as sibling `module`, builds it, then runs Kitchen Sink install/compile.
- Do not manually edit e2e snapshots. Regenerate with `./run test:waspc:e2e:accept-all` from `waspc/`.

## Known Limitations

- Operation extraction in `Wasp.Project.Module` is regex-based.
- The shim only knows operation imports. Other `wasp/*` subpaths remain runtime-only unless explicitly added.

## Verification Commands

- `./run build`
- `npm run test` from `data/packages/module-builder/`
- `../../waspc/run wasp-cli module install && npm run typecheck && ../../waspc/run wasp-cli module build` from `examples/module/`
- `../../waspc/run wasp-cli install && ../../waspc/run wasp-cli compile` from `examples/kitchen-sink/`
- `./run test:waspc:e2e:accept-all`
