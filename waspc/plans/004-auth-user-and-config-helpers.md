# Auth User And Config Helpers

Type: AFK

User stories covered: 1, 2, 3, 4, 5, 7, 22, 24, 27

## Parent

`auth-libs-migration-prd.md`

## What to build

Move generic auth user helpers and OAuth provider config merge behavior into the auth library. Generated code should keep Wasp SDK type wiring and provider-specific configuration imports.

## Acceptance criteria

- [ ] Auth user helper behavior lives in the auth library where it is not tied to generated Wasp modules.
- [ ] Provider config merge behavior lives in the auth library.
- [ ] Generated code keeps Wasp-specific type imports and public API barrels.
- [ ] Tests cover email extraction, username extraction, first provider ID lookup, null user behavior, empty identity behavior, and config override behavior.
- [ ] Runtime boundaries are correct for shared code.
- [ ] Lib tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `001-auth-libs-baseline.md`
