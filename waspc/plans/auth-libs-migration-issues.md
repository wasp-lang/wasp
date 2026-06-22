# Auth Libs Migration Issues

Parent PRD: `auth-libs-migration-prd.md`

## Breakdown

1. `001-auth-libs-baseline.md` - AFK - docs, stale README, current export smoke tests.
2. `002-auth-validation-and-response-schemas.md` - AFK - move validation and response schemas to the auth lib.
3. `003-auth-provider-data-helpers.md` - AFK - move provider ID and provider data helpers to the auth lib.
4. `004-auth-user-and-config-helpers.md` - AFK - move auth user helpers and provider config merge logic.
5. `005-oauth-state-and-cookie-primitives.md` - AFK - move OAuth state and cookie primitives behind adapters.
6. `006-auth-service-adapter-contracts.md` - HITL - agree standalone-lib adapter contract before moving route logic.
7. `007-username-auth-service.md` - AFK - migrate username signup/login internals through adapters.
8. `008-email-login-signup-service.md` - AFK - migrate email login/signup internals through adapters.
9. `009-email-verification-reset-service.md` - AFK - migrate email verification and password reset internals.
10. `010-oauth-flow-service.md` - AFK - migrate OAuth callback, user resolution, and one-time-code internals.
11. `011-headless-auth-ui-api-and-storybook.md` - HITL - define composable headless Auth UI API and Storybook foundation.
12. `012-generated-auth-ui-headless-wrapper.md` - AFK - make generated Auth UI use headless hooks while preserving visuals.
13. `013-drop-in-auth-ui-doc-examples.md` - AFK - add Tailwind and plain CSS drop-in examples matching current Auth UI.

## Dependency Graph

- 001 has no blockers.
- 002, 003, 004 are blocked by 001.
- 005 is blocked by 002 and 003.
- 006 is blocked by 002, 003, 004, 005.
- 007, 008, 009, 010 are blocked by 006.
- 011 is blocked by 001.
- 012 is blocked by 011.
- 013 is blocked by 011 and should be finalized after 012 proves compatibility.
