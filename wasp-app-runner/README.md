# Wasp Application Runner

A robust script to run your Wasp application with PostgreSQL, including automated database setup and migrations.

## Usage

```
npx wasp-app-runner [--app-path <path-to-app> --skip-cli-install <true/false> --wasp-cli-cmd <wasp-cli-cmd>]
```

This runs the package without installing it globally.

### Arguments Table

| Argument             | Description                                            | Example         |
| -------------------- | ------------------------------------------------------ | --------------- |
| `--app-path`         | Path to your Wasp application directory (default: ".") | `./my-wasp-app` |
| `--skip-cli-install` | Skip installing Wasp CLI (default: false)              | `true`          |
| `--wasp-cli-cmd`     | Wasp CLI command (default: `wasp-cli`)                 | `wasp`          |

## Environment File Handling

The script automatically sets up environment files if headless versions exist:

- If `.env.client.headless` exists ➔ copies to `.env.client`
- If `.env.server.headless` exists ➔ copies to `.env.server`

This allows you to commit template files with default values while keeping actual credentials out of source control. **Note:** it will override any locally modified `.env.client` or `.env.server` files.

## Postgres Configuration

Check the `./db/postgres.ts` file to see the Postgres configuration used.

If Postgres is used, the script automatically sets the `DATABASE_URL` env variable for the `wasp start` command:

```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

## Development

```bash
npm install
# Installs wasp-app-runner globally from the local directory
npm install -g
```

You can also run the script directly:

```bash
node ./src/index.js [--app-path <path-to-app> --skip-cli-install <true/false> --wasp-cli-cmd <wasp-cli-cmd>]
```
