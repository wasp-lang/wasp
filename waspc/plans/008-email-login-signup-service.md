# Email Login And Signup Service

Type: AFK

User stories covered: 1, 2, 3, 4, 6, 8, 22, 23, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Move email login and signup internals into the auth library through the approved adapter contract. Generated code should remain responsible for route registration, request/response handling, email content configuration, and adapter construction.

## Acceptance criteria

- [ ] Email login behavior lives in the auth library behind adapters.
- [ ] Email signup behavior lives in the auth library behind adapters.
- [ ] Existing unverified-email behavior is preserved.
- [ ] Existing duplicate verified signup fake-work behavior is preserved.
- [ ] Existing duplicate unverified signup replacement behavior is preserved.
- [ ] Existing email auto-verification behavior is preserved.
- [ ] Existing hook ordering is preserved.
- [ ] Tests cover invalid credentials, unverified email, duplicate verified signup, duplicate unverified signup, resend throttling, auto-verified signup, email send failure, and hook ordering.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `006-auth-service-adapter-contracts.md`
