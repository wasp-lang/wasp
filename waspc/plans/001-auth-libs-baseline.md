# Auth Libs Baseline

Type: AFK

User stories covered: 1, 2, 3, 4, 20, 21, 22, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Prepare the auth library migration baseline. Capture migration invariants in docs, fix stale auth library documentation, and add smoke coverage for the auth library exports that already exist. This gives later slices a stable starting point and prevents future contributors from following outdated package export docs.

## Acceptance criteria

- [ ] Migration docs describe the behavior-preserving goal, runtime export boundaries, and template-versus-lib ownership rules.
- [ ] Auth library docs match current package exports and versioning behavior.
- [ ] Current node export surface has smoke coverage.
- [ ] Current browser export surface has smoke coverage.
- [ ] Existing auth behavior is unchanged.
- [ ] Lib tests pass.
- [ ] Lib package build passes.

## Blocked by

None - can start immediately.
