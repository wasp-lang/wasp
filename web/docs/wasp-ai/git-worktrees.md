---
title: Working with Git worktrees
---

Git worktrees let you work on several isolated instances of the same repository. This is useful when several coding agents work on the app in parallel. A new worktree contains tracked files, but not ignored files such as `node_modules`, `.wasp`, `.env.server`, and `.env.client`.

## Preparing a worktree

After you create a worktree, first run:

```bash
wasp install
```

This installs the project dependencies. `wasp new` runs this command automatically, but in a fresh clone or worktree you need to run it manually.

Prepare the environment files required by the app. For example, if example files exist:

```bash
cp .env.server.example .env.server
cp .env.client.example .env.client
```

Follow the app's setup instructions or use its secret manager. See [Environment variables](/project/env-vars.md) for Wasp's environment file rules.

## Preparing the database

#### SQLite

SQLite needs no separate database process. Apply the migrations:

```bash
wasp db migrate-dev
```

#### PostgreSQL

Start the database in one terminal:

```bash
wasp start db
```

Then apply the migrations from another terminal:

```bash
wasp db migrate-dev
```

Wasp gives each worktree a unique [development database](/data-model/databases.md#using-the-dev-database-provided-by-wasp) name and Docker volume.

Optionally, if the app's README defines this, run the seed command:

```bash
wasp db seed <seed-name>
```

You can now start the app:

```bash
wasp start
```

## Running worktrees at the same time

By default, each app uses ports `3000` and `3001`, and each managed PostgreSQL database uses port `5432`. These ports prevent multiple worktrees from running at the same time.

Give each app different client and server ports. Configure the client port in [`vite.config.ts`](/project/custom-vite-config.md#custom-dev-server-port), then set the related environment variables:

```env title=".env.server"
PORT=4001
WASP_SERVER_URL=http://localhost:4001
WASP_WEB_CLIENT_URL=http://localhost:4000
```

```env title=".env.client"
REACT_APP_API_URL=http://localhost:4001
```

Wasp's managed PostgreSQL database requires port `5432`. To run several PostgreSQL worktrees, manually provision a separate database for each worktree and set its `DATABASE_URL` in `.env.server`.

## Using `.worktreeinclude` file

Some coding tools (e.g., [Claude Code](https://code.claude.com/docs/en/worktrees#copy-gitignored-files-into-worktrees), [Codex](https://learn.chatgpt.com/docs/environments/git-worktrees#copy-ignored-local-files-into-managed-worktrees), and [Conductor](https://www.conductor.build/docs/reference/worktreeinclude)) read a `.worktreeinclude` file and copy matching ignored files into new worktrees:

```gitignore title=".worktreeinclude"
.env.server
.env.client
```

`.worktreeinclude` is not a Git feature, and support differs between tools.
