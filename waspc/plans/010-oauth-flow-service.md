# OAuth Flow Service

Type: AFK

User stories covered: 1, 2, 3, 4, 6, 7, 10, 22, 23, 24, 27

## Parent

`auth-libs-migration-prd.md`

## What to build

Move OAuth callback, OAuth user resolution, redirect result construction, and one-time-code exchange internals into the auth library through the approved adapter contract. Generated code should keep provider-specific SDK calls, router wiring, redirect paths, and adapter construction.

## Acceptance criteria

- [ ] OAuth callback behavior lives in the auth library behind adapters.
- [ ] OAuth existing-user and new-user branches live in the auth library behind adapters.
- [ ] One-time-code exchange behavior lives in the auth library behind adapters.
- [ ] Generated provider config remains generated glue.
- [ ] Existing redirect behavior is preserved.
- [ ] Existing hook ordering is preserved.
- [ ] Tests cover provider token failure, provider profile failure, existing user login, new user signup, redirect result construction, code reuse, missing code, invalid code, and hook ordering.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `006-auth-service-adapter-contracts.md`
