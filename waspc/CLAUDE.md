# Wasp Build Commands and Style Guide

## Essential Commands
- `./run build` - Build the project
- `./run install` - Install locally
- `./run test` - Run all tests
- `./run test:unit [pattern]` - Run unit tests (optional pattern)
- `./run test:cli` - Run CLI tests
- `./run test:e2e` - Run end-to-end tests
- `./run wasp-cli <args>` - Run wasp executable
- `./run code-check` - Format, lint, and analyze code
- `./run ormolu:format` - Format code with Ormolu
- `./run ghcid` - Watch files for changes & report errors

## Code Style
- Imports: Alphabetized, grouped by qualified/unqualified
- Naming: CamelCase for types, camelCase for functions/variables
- Functions: Explicit type signatures for all top-level functions
- Formatting: 2-space indentation, ~100-120 char line width
- Error handling: Either monad, custom error types, detailed messages
- Documentation: Use -- for line comments with a space after