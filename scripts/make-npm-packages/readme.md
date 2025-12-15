# make-npm-packages

A tool for creating platform-specific npm packages for distributing native executables. This tool generates a main npm package and multiple sub-packages, one for each target platform (OS/CPU/libc combination).

## Overview

This tool solves the problem of distributing native executables through npm. It creates:

1. **Main package** - The package users install, which contains a CLI wrapper
2. **Sub-packages** - Platform-specific packages containing the actual executables

The main package uses npm's `optionalDependencies` to install the correct platform-specific sub-package based on the user's system. The CLI wrapper then detects and runs the appropriate executable.

## Architecture

### Main Package

- Contains a CLI wrapper (`bin.js`) that detects the current platform, and through a `data.json` file (injected at package generation time), finds and imports it.
- Executes the native binary with proper environment variables for the data directory
- Declares all sub-packages as optional dependencies (so all the other non-matching platforms doesn't cause installation failure)

### Sub-packages

- Platform-specific packages (e.g., `@wasp.sh/wasp-cli-linux-x64-musl`)
- Each contains a tarball extracted with the native executable
- Uses npm's `os`, `cpu`, and `libc` fields to ensure correct installation

## Usage

```bash
node src/index.ts \
  --input-dir <path-to-input> \
  --output-dir <path-to-output>
```

### Options

- `-h, --help` - Show help and exit
- `--input-dir` (required) - Directory containing `data.json` and tarballs
- `--output-dir` (required) - Directory where packages will be created
- `--main-package-name` - Name of the main package (default: `@wasp.sh/wasp-cli`)
- `--sub-package-name` - Template for sub-package names (default: `@wasp.sh/wasp-cli-$os-$cpu-$libc`)
  - Must include placeholders: `$os`, `$cpu`, `$libc`

### Input Format

The input directory must contain a `data.json` file with the following structure:

```json
{
  "version": "1.0.0",
  "tarballs": [
    {
      "fileName": "path/to/waspc-darwin-arm64-cli.tar.gz",
      "target": ["darwin", "arm64"]
    },
    {
      "fileName": "path/to/waspc-linux-x64-musl-cli.tar.gz",
      "target": ["linux", "x64", "musl"]
    }
  ]
}
```

Each tarball entry specifies:

- `fileName` - Path to the tarball (relative to input-dir)
- `target` - Object of `{os, cpu, libc?}` identifying the platform

### Output

The tool creates:

1. One directory per sub-package, containing:
   - `package.json` with platform constraints
   - `main.js` entry point
   - Extracted tarball contents
2. One directory for the main package, containing:
   - `package.json` with all sub-packages as optional dependencies
   - `bin.js` CLI wrapper
   - `CLIError.js` error handling utility
   - `data.json` with sub-package metadata

You can then run `npm publish` in each package directory to publish them to npm.

## Development

The generator script is written in TypeScript. It is run through the native Node.js TypeScript support in v22.18+, so no compilation step is needed.

The templates themselves are in the `templates/` directory. You can modify them as needed to change the behavior of the generated packages. They are written in regular JavaScript, but have JSDoc comments for type hinting during development.
