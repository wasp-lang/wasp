# Lib Migration

Durable notes for moving generated-app logic from templates into libraries under `data/Generator/libs/`.

## Core Rules

- Preserve current generated-app behavior during migrations.
- Keep runtime boundaries explicit: neutral exports for shared pure logic, node exports for server-only logic, browser exports for browser-only logic.
- Keep generated templates as Wasp glue: generated imports, enabled providers, entity names, Prisma adapters, route wiring, app config, and user hook wiring.
- Design libs as standalone code where reasonable. Generated templates are one consumer, not design owner.
- Keep files small and grouped by concern. Split before creating god files.
- Prefer pure functions and factory functions with closures over classes. Classes are reserved for blessed patterns such as errors.
- Use domain-language adapter contracts instead of generated Wasp module shapes, Prisma types, Express types, or Wasp errors.
- Cover moved logic with lib tests using fake adapters. Generated-app tests should focus on integration glue.

## Auth Lib Direction

- `@wasp.sh/lib-auth` owns reusable auth behavior that can be type-checked and tested without generating an app.
- Templates stay responsible for Wasp-specific auth glue.
- Move pure helpers first, then OAuth state/cookie mechanics, then route internals through adapters.
- Do not combine auth logic migration with Lucia removal, cookie/session redesign, Arctic upgrades, BYO provider APIs, or Auth UI visual redesign.
- Auth UI should expose headless hooks and tiny unstyled primitives. Do not ship a default CSS file from the auth library.
