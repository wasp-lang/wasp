# Testing the --db-image Feature

This document demonstrates how to use the new `--db-image` argument for the wasp app runner.

## Feature Summary

The `--db-image` argument allows users to specify a custom PostgreSQL Docker image when running Wasp applications. This is particularly useful for:

1. **Using specific PostgreSQL versions**: `--db-image postgres:15`
2. **Using PostgreSQL with extensions**: 
   - TimescaleDB: `--db-image timescale/timescaledb:latest-pg16`
   - PostGIS: `--db-image postgis/postgis:14-3.2`
   - pgvector: `--db-image pgvector/pgvector:pg16`

## Usage Examples

### Development Mode

```bash
# Use default PostgreSQL 16
run-wasp-app dev --path-to-app ./my-app

# Use PostgreSQL 15
run-wasp-app dev --path-to-app ./my-app --db-image postgres:15

# Use TimescaleDB for time-series data
run-wasp-app dev --path-to-app ./my-app --db-image timescale/timescaledb:latest-pg16

# Use PostGIS for spatial data
run-wasp-app dev --path-to-app ./my-app --db-image postgis/postgis:14-3.2
```

### Build Mode

```bash
# Use custom image in build mode
run-wasp-app build --path-to-app ./my-app --db-image postgres:15-alpine
```

## Implementation Details

The implementation modifies the following components:

1. **Argument Parser** (`src/args.ts`): Added `--db-image` option
2. **Main Entry** (`src/index.ts`): Passes dbImage through to mode handlers
3. **Dev Mode** (`src/dev/index.ts`): Forwards dbImage to setupDb
4. **Build Mode** (`src/build/index.ts`): Forwards dbImage to setupDb
5. **Database Setup** (`src/db/index.ts`): Passes dbImage to PostgreSQL setup
6. **PostgreSQL Setup** (`src/db/postgres.ts`): Uses custom image or defaults to `postgres:16`

## Testing the Feature

To test this feature with a real Wasp application:

```bash
# Navigate to an example app
cd examples/tutorials/TodoApp

# Run with custom PostgreSQL image
npx run-wasp-app dev --db-image postgres:15

# The logs should show:
# "Starting the PostgreSQL container with image: postgres:15..."
```

## Benefits

- **Flexibility**: Users can choose the PostgreSQL version that matches their production environment
- **Extensions**: Support for specialized PostgreSQL distributions with built-in extensions
- **Compatibility**: Helps with testing against different PostgreSQL versions
- **Development**: Enables using the same database configuration as production

## Backward Compatibility

The feature is fully backward compatible:
- If `--db-image` is not specified, the default `postgres:16` image is used
- Existing scripts and workflows continue to work without modification