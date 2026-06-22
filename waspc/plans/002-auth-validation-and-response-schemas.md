# Auth Validation And Response Schemas

Type: AFK

User stories covered: 1, 2, 3, 4, 5, 22, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Move auth validation rules and response schema definitions into the auth library. Generated code should keep Wasp-specific error mapping and public SDK wiring, while the library owns validation behavior and schema behavior with isolated tests.

## Acceptance criteria

- [ ] Email, username, password, and token validation behavior lives in the auth library.
- [ ] Session and success response schema behavior lives in the auth library.
- [ ] Generated code still throws the same Wasp-facing errors for invalid input.
- [ ] Validation tests cover required fields, email shape, password length, password number requirement, and token presence.
- [ ] Schema tests cover valid and invalid session and success payloads.
- [ ] Runtime boundaries are correct for neutral code.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `001-auth-libs-baseline.md`
