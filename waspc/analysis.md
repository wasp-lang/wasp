# Full-stack module server bundling

## Problem

Full-stack modules declare `wasp` as a peer dependency, but the generated server currently evaluates two copies of stateful Wasp runtime code:

- Rollup bundles the host's generated `wasp/server/jobs` implementation into `server.js`.
- Rollup leaves the installed full-stack module external because it resolves under `node_modules`.
- The external module loads `wasp/server/jobs` through Node at runtime.

The host starts PgBoss in the bundled copy. A module API submitting a module job waits on the other copy's unresolved `pgBossStarted` promise.

A peer dependency guarantees compatible package resolution, not a single evaluation when one consumer is inside a bundle and another remains external.

## Decision

The generated server bundles direct app dependencies that declare `wasp` as a peer dependency. Their bare third-party dependencies remain external.

This makes a full-stack module's server entry points and `wasp/*` imports part of the same Rollup graph as the host runtime:

```text
@kitchen-sink/module/moduleApiServer  bundled
wasp/server/jobs                      bundled once
quote-lib                             external
pg-boss                               external
```

The package's `peerDependencies.wasp` entry is the module marker. The generated Rollup configuration discovers matching direct dependencies once and uses a static package allowlist while building. This avoids per-import package resolution and adding module provenance to AppSpec.

Transitive full-stack module composition is intentionally unsupported until there is a concrete requirement. A full-stack module contributing a spec to an app must be installed as a direct app dependency.

## Risks

- Bundling changes package semantics for code that relies on its installed filesystem location, package-local assets, `import.meta.url`, or runtime-computed imports.
- A non-module package that declares a `wasp` peer will also be bundled. That is currently acceptable because it is explicitly integrating with the Wasp runtime.
- Package discovery assumes Wasp's npm workspace layout, where direct app dependencies are installed under the app's root `node_modules` directory.
- A module dependency nested only below that module's own `node_modules` directory may not resolve after the module entry point is bundled. This remains an explicit limitation until a real package requires dependency relocation or recursive bundling.
- Different incompatible Wasp peer ranges are still a package-installation and validation concern; bundling does not solve version compatibility.
- This fixes duplicate server runtime evaluation for full-stack modules, but does not establish a general capability-injection boundary for host services.

Bare dependencies of the bundled package remain external, limiting the risk for native dependencies and packages with Node-specific loading behavior.

## Rejected alternatives

- Rollup `dedupe: ["wasp"]` cannot affect imports inside a package that Rollup leaves external.
- A process-global PgBoss registry fixes this specific singleton but leaves other duplicated Wasp runtime state possible.
- Injecting job submitters through API context creates a cleaner module boundary, but requires public spec, AppSpec, generator, and type-system changes.
- Externalizing the generated Wasp SDK would restore Node module-cache identity, but conflicts with current server bundling and production packaging assumptions.

## Acceptance criteria

- The server bundle contains module server code instead of an external module entry-point import.
- The module and host share one `pgBossStarted` lifecycle.
- Module third-party dependencies remain external.
- A module-owned API can submit a module-owned job and return its job ID.
- Existing host-owned jobs continue to work.
- Development and production server builds both resolve the module package correctly.
