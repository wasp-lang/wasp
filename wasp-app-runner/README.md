# Wasp Application Runner

A robust script to run your Wasp application, including automated database setup and migrations.

## Usage

```bash
npm install
# Installs wasp-app-runner globally from the local directory
npm run install:global

# Use the wasp-app-runner command to run your Wasp app
wasp-app-runner [--path-to-app <path>] [--build-wasp-cli <true|false>] [--wasp-cli-cmd <command>]
```

### Arguments Table

| Argument           | Description                                            | Example         |
| ------------------ | ------------------------------------------------------ | --------------- |
| `--path-to-app`    | Path to your Wasp application directory (default: ".") | `./my-wasp-app` |
| `--build-wasp-cli` | Build Wasp CLI from source (default: true)             | `false`         |
| `--wasp-cli-cmd`   | Wasp CLI command (default: `wasp-cli`)                 | `wasp`          |

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

### Development

When developing you can run the script directly from the local directory without installing it globally:

```
npm install
npm run start -- [--path-to-app <path>] [--build-wasp-cli <true|false>] [--wasp-cli-cmd <command>]
```

`npm run start` runs `npm run build` to build the TypeScript code and then runs the `./bin/index.js` script.
