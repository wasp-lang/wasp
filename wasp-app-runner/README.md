# Wasp Application Runner

A robust script to run your Wasp application, including automated database setup and migrations. There are two modes:

- `dev`: Runs the Wasp app in development mode using `wasp start`.
- `build`: Builds the Wasp app for production using `wasp build` and runs it.

When installed, it provides a `run-wasp-app` command that can be used to run your Wasp app.

## Usage

```bash
npm install
# Installs wasp-app-runner globally from the local directory
npm run install:global

# Use the `run-wasp-app` command to run your Wasp app
cd /path/to/your/app
run-wasp-app dev
```

### Options

```bash
run-wasp-app <mode> [--path-to-app <path>] [--wasp-cli-cmd <command>]
```

You must pass the `<mode>` as an argument, which can be either `dev` or `build`.

| Option           | Description                                            | Example         |
| ---------------- | ------------------------------------------------------ | --------------- |
| `--path-to-app`  | Path to your Wasp application directory (default: ".") | `./my-wasp-app` |
| `--wasp-cli-cmd` | Wasp CLI command (default: `wasp`)                     | `wasp-cli`      |

## Postgres Configuration

Check the `./db/postgres.ts` file to see the Postgres configuration used.

If Postgres is used, the script automatically sets the `DATABASE_URL` env variable for the server app:

```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

### Env variables

When using the `dev` mode:

- If the app you are trying to run has `.env.server` or `.env.client` env files defined, the `wasp start` will use them as expected.

When using the `build` mode:

- `run-wasp-app` will use the `.env.server` file when running the server container with Docker locally (which is not done usually by Wasp).
- `.env.client` will not be used when building the client, the `REACT_APP_API_URL` is hard-coded to `http://localhost:3001`.

### Development

When developing, you can run the script directly from the local directory without installing it globally:

```
npm install
npm run start -- <mode> [--path-to-app <path>] [--wasp-cli-cmd <command>]
```

`npm run start` runs `npm run build` to build the TypeScript code and then runs the `./bin/index.js` script.
