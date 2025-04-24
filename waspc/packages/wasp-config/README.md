# Wasp Config

Wasp Config is a TypeScript SDK for working with Wasp application specifications. It provides type definitions and utilities that mirror the AppSpec declarations implemented in the `waspc` Haskell codebase.

## Important Notes

The type definitions in this package are meant to mirror the AppSpec declarations defined in the `waspc` Haskell codebase. When making changes to this package, ensure that they align with the corresponding Haskell implementation.

## Installation

```bash
npm install wasp-config
```

## Testing 

### `npm run test`

Runs all test suites using `vitest`. 

### `npm run test:unit`

Runs only the unit tests using `vitest`. 

### `npm run test:integration`

Runs only the integration tests using `vitest`. 

### `npm run coverage`

Runs the test suite with coverage reporting enabled. This generates a detailed report of how much of the codebase is covered by tests, helping identify areas that might need additional testing.