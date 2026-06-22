# Generated Auth UI Headless Wrapper

Type: AFK

User stories covered: 3, 4, 11, 15, 16, 17, 18, 19, 22, 23, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Make the existing generated Auth UI use headless auth library behavior while preserving the current user-facing visuals and public generated components. This gives Wasp a compatibility wrapper and proves the headless API works in generated apps.

## Acceptance criteria

- [ ] Existing generated Auth UI public imports keep working.
- [ ] Current Auth UI visuals remain equivalent.
- [ ] Current loading, error, and success behavior remains equivalent.
- [ ] Current email, username, and OAuth UI flows remain equivalent where enabled.
- [ ] Generated UI wrappers call headless auth library hooks or primitives.
- [ ] Generated templates become thinner.
- [ ] Storybook compatibility example reflects the generated wrapper behavior where useful.
- [ ] Browser/headless UI tests pass.
- [ ] Generated auth integration still builds.

## Blocked by

- `011-headless-auth-ui-api-and-storybook.md`
