# Wasp Application Runner

A robust script to manage your Wasp application with PostgreSQL integration, including automated database setup and migrations.

## Installation

```
npm install
```

## Usage

```
node run-app.js --app-path <path-to-app> --app-name <app-name>
```

### Arguments Table

| Argument     | Description                             | Example         |
| ------------ | --------------------------------------- | --------------- |
| `--app-path` | Path to your Wasp application directory | `./my-wasp-app` |
| `--app-name` | Unique name for your application        | `myapp`         |

### Example Command

```
node run-app.js --app-path ~/projects/my-wasp-app --app-name awesome-app
```

## Environment File Handling

The script automatically sets up environment files if headless versions exist:

- If `.env.client.headless` exists ➔ copies to `.env.client`
- If `.env.server.headless` exists ➔ copies to `.env.server`

This allows you to commit template files with default values while keeping actual credentials out of source control. **Note:** it will override any locally modified `.env.client` or `.env.server` files.

## Configuration Details

- Port: 5432
- Password: `devpass`
- Docker image: `postgres:16`
- Container name template: `<app-name>-postgres`
- Health check retries: 10
- Health check delay: 2000ms

## Environment Variables

The script automatically sets:

```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

## Workflow Steps

1. Dependency verification
2. PostgreSQL container setup
3. Database readiness checks
4. Migration execution
5. Application startup
