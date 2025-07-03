# Wasp Config

Wasp Config is a library for defining a Wasp application specification in Typescript. It provides type definitions and utilities that mirror the `AppSpec` declarations implemented in the `waspc` Haskell codebase.

## Important Notes

The `appSpec.ts` type definitions in this package are meant to mirror the AppSpec declarations defined in the `waspc` Haskell codebase. When making changes to this package, ensure that they align with the corresponding Haskell implementation.

## Testing

```bash
npm run test
npm run test:unit
npm run test:integration
npm run coverage
```
