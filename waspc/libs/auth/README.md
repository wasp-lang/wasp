# Wasp Auth Library

This library exports code related to Wasp's auth. It has multiple exports:

- `sdk` - used in the Wasp SDK in any runtime
- `sdk/browser` - used in the Wasp SDK in the browser runtime
- `server` - used in the `server` app

## Setup

Install the dependencies:

```bash
npm install
```

## Development

Start unit tests in watch mode:

```bash
npm run dev
```

Develop code and write unit tests for it.

## Coverage

To check the code coverage, run:

```bash
npm run test
```

## Building the library

To build the library, run:

```bash
npm run build
```

## Verifying library can be consumed

To check the type exports and ensure the library can be consumed, run:

```bash
npx @arethetypeswrong/cli -P
```

You want to see `🟢` for all items and `⚠️ ESM (dynamic import only) ` for `node16`.

Another test you can run:

```bash
npx publint
```

`publint` should show no errors.
