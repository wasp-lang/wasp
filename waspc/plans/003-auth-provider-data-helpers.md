# Auth Provider Data Helpers

Type: AFK

User stories covered: 1, 2, 3, 4, 5, 8, 9, 22, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Move provider ID normalization and provider data serialization behavior into the auth library. Generated code should keep database calls and entity-specific types, while the library owns provider identity and provider data behavior.

## Acceptance criteria

- [ ] Provider ID creation and normalization live in the auth library.
- [ ] Email and username provider IDs keep current lowercasing behavior.
- [ ] OAuth provider IDs keep current identity-preserving behavior.
- [ ] Provider data parsing, serialization, password hashing, and password redaction live in the auth library.
- [ ] Generated code still owns persistence and entity-specific types.
- [ ] Tests cover provider ID normalization across all current providers.
- [ ] Tests cover provider data serialization, sanitized reads, password-preserving reads, and password hashing.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `001-auth-libs-baseline.md`
