# Changelog

## v0.3.0.0 (2022/02/04)

### [BREAKING CHANGE] New Wasp-lang syntax!

Mostly it is very similar to what it was before, with some following bigger changes:
  - `auth`, `dependencies`, and couple of other "singleton" delcarations now became part of `app` declaration.
  - All declarations now need to have name, including `route`.
  - `route` has different syntax.
  - `dependencies` have different syntax.

For exact details about new syntax, check https://wasp-lang.dev/docs/language/syntax .

### Various improvements

  - Better compiler error messages.
  - Nicer CLI output.
  - Added delay on recompilation to avoid redundant recompiling.
  - Added `onAuthSucceededRedirectTo` field in `app`.
  - and more!

## Unreleased changes
