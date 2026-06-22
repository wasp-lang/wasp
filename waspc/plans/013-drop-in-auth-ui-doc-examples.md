# Drop-In Auth UI Docs Examples

Type: AFK

User stories covered: 13, 14, 16, 17, 18, 19, 20, 21

## Parent

`auth-libs-migration-prd.md`

## What to build

Update Wasp docs with drop-in Auth UI examples that use the headless auth library API and visually match current Auth UI. Provide both Tailwind and plain CSS versions. The migration path should feel like swapping generated component imports for hook imports, then pasting explicit markup and styling.

## Acceptance criteria

- [ ] Docs include a Tailwind drop-in login example matching current Auth UI.
- [ ] Docs include a Tailwind drop-in signup example matching current Auth UI.
- [ ] Docs include Tailwind examples for forgot password, reset password, and verify email.
- [ ] Docs include plain CSS equivalents for the same flows.
- [ ] Examples include loading, errors, success messages, disabled state, OAuth provider buttons, and exported provider icon primitives where appropriate.
- [ ] Examples explain that generated Auth UI remains available.
- [ ] Examples explain that headless hooks are the customization path.
- [ ] Examples do not require `@wasp.sh/lib-auth` to ship CSS.
- [ ] Docs build passes.
- [ ] Visual review confirms examples match current Auth UI closely enough for seamless migration.

## Blocked by

- `011-headless-auth-ui-api-and-storybook.md`
- `012-generated-auth-ui-headless-wrapper.md`
