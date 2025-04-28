# Wasp Application Runner

A robust script to run your Wasp application, including automated database setup and migrations.

## Usage

```bash
npm install
# Installs wasp-app-runner globally from the local directory
npm run install:global

# Use the wasp-app-runner command to run your Wasp app
wasp-app-runner run <mode> [--path-to-app <path>] [--wasp-cli-cmd <command>]
```

### Options Table

TODO: write about the `<mode>` argument (`dev`, `build`)

| Option           | Description                                            | Example         |
| ---------------- | ------------------------------------------------------ | --------------- |
| `--path-to-app`  | Path to your Wasp application directory (default: ".") | `./my-wasp-app` |
| `--wasp-cli-cmd` | Wasp CLI command (default: `wasp`)                     | `wasp-cli`      |

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
npm run start -- [--path-to-app <path>] [--wasp-cli-cmd <command>]
```

`npm run start` runs `npm run build` to build the TypeScript code and then runs the `./bin/index.js` script.
