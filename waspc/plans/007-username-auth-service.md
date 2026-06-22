# Username Auth Service

Type: AFK

User stories covered: 1, 2, 3, 4, 6, 9, 22, 23, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Move username signup and login internals into the auth library through the approved adapter contract. Generated code should remain responsible for route registration, request/response handling, and adapter construction.

## Acceptance criteria

- [ ] Username signup behavior lives in the auth library behind adapters.
- [ ] Username login behavior lives in the auth library behind adapters.
- [ ] Generated code remains thin route glue.
- [ ] Existing hook ordering is preserved.
- [ ] Existing invalid credentials behavior is preserved.
- [ ] Tests cover signup, duplicate signup, invalid credentials, failed password verification, session creation, and hook ordering.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `006-auth-service-adapter-contracts.md`
