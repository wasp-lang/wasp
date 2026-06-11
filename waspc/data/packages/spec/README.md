# @wasp.sh/spec

Wasp Spec is a library for defining a Wasp application specification in Typescript. It provides type definitions and utilities that mirror the `AppSpec` declarations implemented in the `waspc` Haskell codebase.

## Important Notes

The `appSpec.ts` type definitions in this package are meant to mirror the AppSpec declarations defined in the `waspc` Haskell codebase. When making changes to this package, ensure that they align with the corresponding Haskell implementation.

When linking to other Wasp docs from TSDoc comments in this package, write the full `https://wasp.sh/docs/...` URL rather than a relative path. The website build rewrites those URLs to relative links via the `fix-api-links` remark plugin (see [Writing Docs](../../../../web/WRITING-DOCS.md)), so they resolve correctly both on the rendered website and in the raw markdown that consumers of this package read.

## Testing

```bash
npm run test
npm run test:unit
npm run test:integration
npm run coverage
```
