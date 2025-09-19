# Testing the --db-image Feature for Railway Deployment

## Summary
Added support for an optional `--db-image` argument to `wasp deploy railway` commands (setup and launch), allowing users to define alternative Postgres images with extensions.

## Changes Made

### 1. Added `--db-image` option to CLI commands
- Updated `makeRailwaySetupCommand()` in `/waspc/packages/deploy/src/providers/railway/index.ts`
- Updated `makeRailwayLaunchCommand()` in the same file
- Option accepts a Docker image name (e.g., `postgis/postgis`, `pgvector/pgvector`)

### 2. Updated SetupCmdOptions interface
- Added `dbImage?: string` to `/waspc/packages/deploy/src/providers/railway/commands/setup/SetupCmdOptions.ts`

### 3. Enhanced database setup logic
- Modified `setupDb()` function in `/waspc/packages/deploy/src/providers/railway/commands/setup/setup.ts`
- When `--db-image` is provided:
  - Creates a custom Docker service with the specified image
  - Configures necessary PostgreSQL environment variables
  - Sets up DATABASE_URL for Railway's connection
- When not provided, falls back to Railway's default Postgres template

## Usage Examples

### Using PostGIS extension
```bash
wasp deploy railway setup my-app --db-image postgis/postgis:16-3.4
```

### Using pgvector for embeddings
```bash
wasp deploy railway launch my-app --db-image pgvector/pgvector:pg16
```

### Using default Postgres (no change from existing behavior)
```bash
wasp deploy railway setup my-app
```

## Implementation Details

When a custom image is specified, the setup function:
1. Creates a service with the specified Docker image using `railway add --docker-image`
2. Configures PostgreSQL environment variables:
   - `POSTGRES_DB=railway`
   - `POSTGRES_USER=postgres`
   - `POSTGRES_PASSWORD=${{secret()}}` (Railway generates a secure password)
   - `DATABASE_URL` with Railway's TCP proxy configuration

## Verification
The implementation has been successfully built and the options appear in the CLI help:
```bash
$ node ./dist/index.js railway setup --help
# Shows: --db-image [dbImage]  custom Docker image for the Postgres database...

$ node ./dist/index.js railway launch --help  
# Shows: --db-image [dbImage]  custom Docker image for the Postgres database...
```