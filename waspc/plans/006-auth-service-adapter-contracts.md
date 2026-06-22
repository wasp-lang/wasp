# Auth Service Adapter Contracts

Type: HITL

User stories covered: 1, 2, 3, 4, 6, 8, 9, 10, 20, 21, 22, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Define the adapter contract that lets route internals move into the auth library without importing generated Wasp code. The auth library should be standalone auth code, with generated templates acting as one consumer. The contract should cover auth persistence, sessions, hooks, email, one-time tokens, time, randomness, fake work, and Wasp-facing errors without encoding generated Wasp module shapes into the library API.

## Acceptance criteria

- [ ] Adapter contract covers current email auth behavior.
- [ ] Adapter contract covers current username auth behavior.
- [ ] Adapter contract covers current OAuth callback and one-time-code behavior.
- [ ] Adapter contract keeps generated Prisma and Wasp modules outside the auth library.
- [ ] Adapter contract treats generated templates as one consumer, not as the library's internal architecture.
- [ ] Adapter contract uses auth-domain capabilities instead of generated Wasp module names or import paths.
- [ ] Adapter contract leaves future standalone use plausible.
- [ ] Adapter contract supports fake implementations for unit tests.
- [ ] Error boundary is explicit: library returns domain errors or calls an injected error factory.
- [ ] Human review confirms the contract before route internals move.
- [ ] No user-facing auth behavior changes in this slice.

## Blocked by

- `002-auth-validation-and-response-schemas.md`
- `003-auth-provider-data-helpers.md`
- `004-auth-user-and-config-helpers.md`
- `005-oauth-state-and-cookie-primitives.md`
