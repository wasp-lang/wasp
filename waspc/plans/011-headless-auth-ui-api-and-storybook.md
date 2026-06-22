# Headless Auth UI API And Storybook

Type: HITL

User stories covered: 11, 12, 15, 16, 17, 18, 19, 20, 21, 22, 24

## Parent

`auth-libs-migration-prd.md`

## What to build

Define the headless Auth UI API and add Storybook coverage for it. The auth library should expose composable hooks and tiny unstyled primitives for form state, submit behavior, loading, errors, success messages, provider actions, and provider icons. It should not ship default CSS, external asset files, or styled provider buttons. The API should be inspired by mature composable UI libraries, but avoid a large Wasp-specific customization API when small pieces and composition are enough.

## Acceptance criteria

- [ ] Headless Auth UI API is reviewed before generated UI migrates to it.
- [ ] Browser exports include headless hooks or primitives for current auth forms.
- [ ] Browser exports prefer small composable hooks and primitives over configuration-heavy components.
- [ ] API does not import generated Wasp modules.
- [ ] API does not require shipped CSS.
- [ ] API supports OAuth provider actions.
- [ ] API exports unstyled provider icon primitives as inline SVG React components.
- [ ] API makes common customization possible by composition, not by a deep customization config object.
- [ ] API ergonomics are compared against mature composable UI library patterns before final review.
- [ ] Storybook renders raw headless examples.
- [ ] Storybook renders Tailwind examples that match current Auth UI direction.
- [ ] Storybook renders plain CSS examples that match current Auth UI direction.
- [ ] Interaction tests cover loading, submit, errors, success, disabled state, and provider callbacks.
- [ ] Lib browser tests pass.

## Blocked by

- `001-auth-libs-baseline.md`
