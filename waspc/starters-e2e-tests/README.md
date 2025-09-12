# Wasp starters E2E tests

This project provides automated end-to-end testing for Wasp starter templates using Playwright.

## Overview

The test suite automatically (for each starter):

1. Sets up the test environment in a temporary directory
2. Creates a new Wasp projects from the starter template
3. Configures the Wasp project for e2e testing
4. Runs the Wasp project in both development and build
5. Validates the project functionality using Playwright

## Installation

```bash
npm install
```

## Usage

### Run tests with production Wasp CLI

```bash
npm run test
```

This uses the `wasp` command.

### Run tests with development Wasp CLI

```bash
npm run test:dev
```

This uses the `wasp-cli` command (typically used for development builds).

### Run tests with custom Wasp CLI command

```bash
npm run build
npm run start -- --wasp-cli-command custom-command-here
```

## Project tests structure

```
├── tests/
│   ├── health-check.spec.ts             # Basic health check tests run for every starter
│   └── starter/                         # Starter-specific test files
```

## License

MIT
