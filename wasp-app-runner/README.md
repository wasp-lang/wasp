# Wasp Application Runner

A robust script to run your Wasp application with PostgreSQL, including automated database setup and migrations.

## Installation

```bash
npm install
# Installs wasp-app-runner globally
npm install -g
```

## Usage

```
node ./src/index.js --app-path <path-to-app> --app-name <app-name> [--db-type <db-type>] [--skip-cli-install <true/false>]
```

### Arguments Table

| Argument             | Description                             | Example         |
| -------------------- | --------------------------------------- | --------------- |
| `--app-path`         | Path to your Wasp application directory | `./my-wasp-app` |
| `--app-name`         | Unique name for your application        | `myapp`         |
| `--db-type`          | Choices: `postgres`, `sqlite`           | `postgres`      |
| `--skip-cli-install` | Skip installing Wasp CLI                | `true`          |

### Example Command

```
node run-app.js --app-path ~/projects/my-wasp-app --app-name awesome-app
```

## Environment File Handling

The script automatically sets up environment files if headless versions exist:

- If `.env.client.headless` exists ➔ copies to `.env.client`
- If `.env.server.headless` exists ➔ copies to `.env.server`

This allows you to commit template files with default values while keeping actual credentials out of source control. **Note:** it will override any locally modified `.env.client` or `.env.server` files.

## Postgres Configuration

- Port: 5432
- Password: `devpass`
- Docker image: `postgres:16`
- Container name template: `<app-name>-postgres`
- Health check retries: 10
- Health check delay: 2000ms

If Postgres is used, the script automatically sets the `DATABASE_URL` env variable for the `wasp start` command:

```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```
