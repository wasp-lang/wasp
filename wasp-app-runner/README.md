# Wasp Application Runner

A robust script to run your Wasp application, including automated database setup and migrations. There are two modes:

- `dev`: Runs the Wasp app in development mode using `wasp start`.
- `build`: Builds the Wasp app for production using `wasp build` and runs it.

When installed, it provides a `run-wasp-app` command that can be used to run your Wasp app.

## Usage

### Global dependency:

```bash
npm install -g @wasp.sh/wasp-app-runner

run-wasp-app dev
```

### Local dependency:

```bash
npm i -D @wasp.sh/wasp-app-runner

npx run-wasp-app dev
```

You can also use it in `npm` scripts without the `npx` prefix, since `node_modules/.bin` is automatically added to the `PATH` inside `scripts`:

Example `package.json`:

```jsonc
{
  // ...
  "scripts": {
    "run-dev": "run-wasp-app dev",
  },
  // ...
}
```

### One time usage:

```bash
npx @wasp.sh/wasp-app-runner dev
```

### Options

```
npx run-wasp-app <mode> [--path-to-app <path>] [--wasp-cli-cmd <command>] [--db-image <image>]
```

You must pass the `<mode>` as an argument, which can be either `dev` or `build`.

| Option           | Description                                            | Example           |
| ---------------- | ------------------------------------------------------ | ----------------- |
| `--path-to-app`  | Path to your Wasp application directory (default: ".") | `./my-wasp-app`   |
| `--wasp-cli-cmd` | Wasp CLI command (default: `wasp`)                     | `wasp-cli`        |
| `--db-image`     | Custom PostgreSQL Docker image (default: `postgres`)   | `postgis/postgis` |

### Postgres Configuration

Check the `./db/postgres.ts` file to see the Postgres configuration used.

If Postgres is used, the script automatically sets the `DATABASE_URL` env variable for the server app:

```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

#### Custom database image

You can override the Docker image used for Postgres via `--db-image`. If not provided, the default `postgres` is used.

Examples: `--db-image postgres:15`, `--db-image pgvector/pgvector:pg16`, `--db-image postgis/postgis:14-3.2`.

> [!NOTE]
> The same requirements as in `wasp start db` apply for the Postgres image used. See [Wasp docs](../web/docs/data-model/databases.md#custom-database) for more details.

### Env variables

When using the `dev` mode:

- If the app you are trying to run has `.env.server` or `.env.client` env files defined, the `wasp start` will use them as expected.

When using the `build` mode:

- `run-wasp-app` will use the `.env.server` file when running the server container with Docker locally (which is not done usually by Wasp).
- `.env.client` will not be used when building the client, the `REACT_APP_API_URL` is hard-coded to `http://localhost:3001`.

## Development

When developing, you can run the script directly from the local directory without installing it globally:

```
npm install
npm run start -- <mode> [--path-to-app <path>] [--wasp-cli-cmd <command>] [--db-image <image>]
```

`npm run start` runs `npm run build` to build the TypeScript code and then runs the `./bin/index.js` script.

## Publishing

To publish a new version of `wasp-app-runner`, follow these steps:

### 1. Update Package Version

**Using npm version command:**

```bash
# For patch releases (bug fixes)
npm version patch

# For minor releases (new features)
npm version minor

# For major releases (breaking changes)
npm version major
```

### 2. Authenticate with npm

```bash
# Login to npm with an account that has access to the @wasp.sh organization
npm login
```

### 3. Publish to npm Registry

```bash
# Publish the package publicly
npm publish --access public
```
