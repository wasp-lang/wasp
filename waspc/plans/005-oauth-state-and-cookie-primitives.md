# OAuth State And Cookie Primitives

Type: AFK

User stories covered: 1, 2, 3, 4, 7, 10, 22, 24, 27

## Parent

`auth-libs-migration-prd.md`

## What to build

Move OAuth state generation, PKCE verifier behavior, callback state validation, and OAuth cookie name/value behavior into the auth library. Generated code should pass request, response, provider, and secure-cookie settings through thin adapters.

## Acceptance criteria

- [ ] OAuth2 state generation and validation behavior lives in the auth library.
- [ ] OAuth2 with PKCE state generation and validation behavior lives in the auth library.
- [ ] Cookie name/value behavior lives in the auth library without importing generated Wasp modules.
- [ ] Generated code keeps Express and config glue.
- [ ] Tests cover valid OAuth2 state, valid PKCE state, missing code, invalid state, missing verifier, cookie names, and secure-cookie settings.
- [ ] OAuth callback behavior is unchanged.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `002-auth-validation-and-response-schemas.md`
- `003-auth-provider-data-helpers.md`
