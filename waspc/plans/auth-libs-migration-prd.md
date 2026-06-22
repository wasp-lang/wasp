# Auth Libs Migration PRD

## Problem Statement

Wasp auth logic still lives mostly in generated Mustache templates. Those templates are hard to type-check, hard to unit test, hard to refactor, and hard for contributors to understand because they are not normal TypeScript projects. This slows Wasp auth development and makes behavior changes riskier.

Wasp already has internal generated-app libraries. The auth library proves the model works, but only a small slice of auth logic has moved there. We need a staged migration plan that moves auth logic into `@wasp.sh/lib-auth` without changing user-facing behavior.

Wasp Auth UI also needs a better migration path. Current generated Auth UI gives users a working styled experience, but moving it into a library should not force Wasp to ship a public default CSS package. Users should be able to use headless auth behavior and copy drop-in examples that look like current Auth UI with either Tailwind or plain CSS.

## Solution

Move auth logic from templates into `@wasp.sh/lib-auth` in small, behavior-preserving phases. The auth library should be standalone auth code, with generated templates acting as one consumer of it. Templates stay responsible for Wasp-specific glue: generated imports, enabled providers, entity names, Prisma adapters, route wiring, app config, and user hooks. The library owns reusable auth behavior that can be type-checked and tested in isolation, and should be designed so it could plausibly be used outside generated Wasp apps in the future.

For Auth UI, ship composable headless hooks and tiny unstyled primitives from the auth library. Keep default styling outside the package. Avoid a large customization API when composition can solve the same problem. Update docs with drop-in Tailwind and plain CSS examples that visually match today's Auth UI, so users can migrate from generated components to hooks by swapping imports and adding explicit code plus styles.

## User Stories

1. As a Wasp maintainer, I want auth logic in a normal TypeScript package, so that I can type-check it without generating an app.
2. As a Wasp maintainer, I want auth logic covered by unit tests, so that I can change auth behavior with confidence.
3. As a Wasp maintainer, I want templates to stay thin, so that generated code is easier to reason about.
4. As a Wasp maintainer, I want to preserve current auth behavior during migration, so that users see no regressions.
5. As a Wasp maintainer, I want pure auth helpers extracted first, so that early migration PRs are low risk.
6. As a Wasp maintainer, I want adapter interfaces around Prisma, sessions, hooks, email, and token storage, so that route behavior can move to the library without importing generated Wasp code.
7. As a Wasp maintainer, I want OAuth state handling tested in isolation, so that provider callback bugs are easier to catch.
8. As a Wasp maintainer, I want email auth flows tested with fake stores and hooks, so that signup, verification, login, and reset behavior stays stable.
9. As a Wasp maintainer, I want username auth flows tested with fake stores and hooks, so that username/password behavior stays stable.
10. As a Wasp maintainer, I want OAuth callback and one-time-code flows tested with fake adapters, so that OAuth changes do not require full generated-app testing for every branch.
11. As a Wasp maintainer, I want Auth UI behavior in headless hooks, so that UI behavior is testable without CSS packaging decisions.
12. As a Wasp maintainer, I want Storybook for headless Auth UI, so that behavior and example compositions are easy to inspect.
13. As a Wasp docs writer, I want drop-in Tailwind examples that look like current Auth UI, so that users can migrate without redesigning their auth pages.
14. As a Wasp docs writer, I want drop-in plain CSS examples that look like current Auth UI, so that non-Tailwind users have a complete path.
15. As a Wasp user, I want current generated Auth UI to keep working, so that upgrades do not break my app.
16. As a Wasp user, I want headless Auth UI hooks, so that I can build custom auth screens without fighting default styles.
17. As a Wasp user, I want examples that match current Auth UI, so that I can get the same visual result after migrating.
18. As a Wasp user, I want OAuth provider buttons shown in docs examples, so that social login remains easy to copy.
19. As a Wasp user, I want error and loading states included in docs examples, so that copied auth screens handle real flows.
20. As a Wasp contributor, I want clear migration phases, so that I can pick up one slice without understanding all auth internals.
21. As a Wasp contributor, I want out-of-scope auth redesigns documented, so that migration PRs do not expand into product changes.
22. As a Wasp contributor, I want lib tests to run in CI, so that extracted logic stays healthy.
23. As a Wasp contributor, I want generated-app tests to remain focused on integration glue, so that snapshots do not become the only safety net.
24. As a Wasp maintainer, I want docs and code to explain runtime boundaries, so that browser-only and node-only auth code does not leak across bundles.
25. As a Wasp maintainer, I want the migration to make future Lucia removal easier, so that dependency cleanup can happen later as a separate task.
26. As a Wasp maintainer, I want the migration to make future cookie/session changes easier, so that auth storage redesign can happen later as a separate task.
27. As a Wasp maintainer, I want the migration to make future OAuth provider work easier, so that adding or updating providers becomes less repetitive.
28. As a Wasp maintainer, I want the auth library to be standalone enough that generated templates are just one consumer, so that future standalone use remains possible.
29. As a Wasp user, I want composable Auth UI primitives instead of a complex customization API, so that I can customize by arranging small pieces rather than learning Wasp-specific styling knobs.

## Implementation Decisions

- The auth library remains an internal Wasp implementation detail versioned with the Wasp compiler.
- The auth library should still be designed as standalone auth code. Generated templates are one consumer, not the owner of the library design.
- The migration must be behavior-preserving. Existing generated Auth UI and auth flows must keep working.
- Runtime boundaries stay explicit: neutral exports for shared pure logic, node exports for server-only logic, browser exports for React/headless UI logic.
- Templates remain responsible for generated app glue: imports, enabled providers, route registration, app config, Prisma access, entity names, public SDK barrels, and user hook wiring.
- The library owns pure helpers first: validation, response schemas, provider ID normalization, provider data serialization, provider config merging, and user identity helpers.
- OAuth state and cookie mechanics move after pure helpers. Generated code passes request, response, provider config, and secure-cookie settings through adapters.
- Route logic moves only after adapter interfaces exist. The library should not import generated Wasp modules.
- Adapter contracts should use domain language and capabilities, not generated Wasp module shapes.
- Email and username auth route internals move behind adapters for stores, sessions, hooks, email, token handling, clock, random, and error creation.
- OAuth callback, OAuth user resolution, and one-time-code exchange move behind adapters after email and username flows prove the adapter model.
- Auth UI migration depends on Storybook and interaction tests.
- Auth UI ships headless behavior, not default styles.
- Auth UI should prefer small composable hooks and primitives over a large customization API.
- No CSS file is exported from the auth library for the headless UI plan.
- The browser API can ship unstyled provider icon primitives. These should be inline SVG React components, not external asset files or styled buttons.
- Existing generated styled Auth UI can become a compatibility wrapper around headless hooks and primitives.
- Headless Auth UI API design should take inspiration from mature composable UI libraries, while staying small enough for Wasp's auth use case.
- Docs must include drop-in Tailwind examples that visually match current Auth UI.
- Docs must include drop-in plain CSS examples that visually match current Auth UI.
- Drop-in examples must cover login, signup, forgot password, reset password, verify email, loading states, success messages, errors, and social provider buttons.
- Drop-in examples should use the exported provider icon primitives where they match current Auth UI.
- Drop-in examples should be written so migration feels like swapping component imports for hook imports plus explicit markup and styling.
- The docs should explain that generated Auth UI remains available, while headless hooks are the path for custom UI.
- Stale auth library docs must be corrected to match current package exports.

## Migration Phases

1. Baseline docs and current export tests.
2. Pure helper extraction.
3. OAuth state and cookie mechanic extraction.
4. Adapter interface introduction without route behavior movement.
5. Email and username auth route logic migration.
6. OAuth flow logic migration.
7. Storybook and headless Auth UI test foundation.
8. Headless Auth UI migration under existing generated wrappers.
9. Docs with drop-in Tailwind and plain CSS Auth UI examples matching current Auth UI.

## Testing Decisions

- Lib tests should cover external behavior, not implementation details.
- Pure helper tests should cover validation rules, response schema parsing, provider ID normalization, provider data serialization, provider data sanitization, password hashing integration, user identity helpers, and config merging.
- OAuth state tests should cover OAuth2, OAuth2 with PKCE, missing code, invalid state, missing verifier, cookie names, and secure-cookie adapter behavior.
- Service adapter tests should use fake stores, fake hooks, fake sessions, fake email, fake token stores, fake time, and fake randomness.
- Email auth tests should cover invalid credentials, unverified email, duplicate verified signup, duplicate unverified signup, resend throttling, auto-verified signup, email send failure, password reset token failure, and hook ordering.
- Username auth tests should cover signup, duplicate signup, invalid credentials, password verification failure, session creation, and hook ordering.
- OAuth flow tests should cover provider token failure, provider profile failure, existing user login, new user signup, redirect result construction, one-time-code reuse, missing code, invalid code, and hook ordering.
- Browser/headless UI tests should cover submit state, loading state, field errors, global errors, success messages, disabled buttons, provider button callbacks, and form reset behavior.
- Browser/headless UI tests should verify composability from public API behavior, not internal state implementation.
- Storybook should include raw headless examples, Tailwind drop-in examples, plain CSS drop-in examples, and compatibility wrapper examples where useful.
- Generated-app tests should verify integration glue: route wiring, provider combinations, generated imports, custom signup fields, hook presence, and auth combinations.
- E2E snapshots must be regenerated only through the project's snapshot accept command.

## Out of Scope

- Removing Lucia.
- Switching auth from localStorage to cookies.
- Redesigning session storage.
- Upgrading Arctic.
- Adding bring-your-own auth provider APIs.
- Redesigning current Auth UI visuals.
- Publishing a public styled Auth UI package.
- Shipping a default auth CSS file from the library.
- Manually editing e2e snapshots.

## Further Notes

- This work supports the broader goal of reducing template footprint while keeping generated code understandable.
- The headless UI decision avoids CSS packaging as a blocker and gives users more control.
- The standalone-lib direction means generated Wasp code should consume the auth library through explicit adapters, not through hidden generated-module dependencies.
- Drop-in docs examples are required for adoption. They should look like current Auth UI so migration is seamless for users who want the same UI.
- Future auth redesigns become easier after this migration, but this PRD intentionally avoids doing those redesigns.
