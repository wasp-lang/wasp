# Email Verification And Password Reset Service

Type: AFK

User stories covered: 1, 2, 3, 4, 6, 8, 22, 23, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Move email verification, password reset request, and password reset completion internals into the auth library through the approved adapter contract. Generated code should remain responsible for route registration, request/response handling, email content configuration, token adapter construction, and Wasp-facing errors.

## Acceptance criteria

- [ ] Email verification behavior lives in the auth library behind adapters.
- [ ] Password reset request behavior lives in the auth library behind adapters.
- [ ] Password reset completion behavior lives in the auth library behind adapters.
- [ ] Existing token validation behavior is preserved.
- [ ] Existing resend throttling behavior is preserved.
- [ ] Existing provider data update behavior is preserved.
- [ ] Tests cover valid token, missing token, invalid token, expired token, used token, resend throttling, password update, and hook behavior where applicable.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `006-auth-service-adapter-contracts.md`
