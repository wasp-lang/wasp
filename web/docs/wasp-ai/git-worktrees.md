---
title: Git worktree setup
sidebar_label: Worktrees
---

Git worktrees let you work on several isolated instances of the same repository. This is useful when several coding agents work on the app in parallel.

## Worktrees

A [Git worktree](https://git-scm.com/docs/git-worktree) is a separate working directory linked to the same Git repository. Each worktree can check out a different branch while sharing the repository history.

You can create a worktree from `main` with Git:

```bash
git worktree add -b feature/my-feature ../my-app-feature main
```

Tools such as [Herdr](https://github.com/herdrdev/herdr), [Workmux](https://github.com/raine/workmux), and [Conductor](https://www.conductor.build/) can create and manage worktrees for you.

## Setting up a Wasp worktree

Creating the worktree is only the first step. Each worktree is a fresh copy of the app, so you also need to install its dependencies, configure its environment variables, and prepare its database.

### Installing dependencies

After you create a new worktree for your Wasp app, you must first run:

```bash
wasp install
```

It installs the Wasp app dependencies. When creating a new project, `wasp new` installs them for you, but in a fresh clone or worktree you need to run it manually.

### Setting up environment variables {#setting-up-environment-variables}

Most apps need environment variables set up before they work. We can set up the server and client environment variables in different ways:

- Copy the example environment files to `.env.server` and `.env.client`.

    ```bash
    cp .env.server.example .env.server
    cp .env.client.example .env.client
    ```

    Sometimes, this is enough to get the app running if the example files contain dummy values but features like OAuth might not work.

- Copy the environment files from the existing `.env.server` and `.env.client` files.

    Reusing these files is convenient, but it also reuses all configured secrets and services. Make sure this does not cause unintended side effects, such as connecting to a production database.

- If you are using a secrets manager like [Dotenvx](https://dotenvx.com/), use its CLI to set up the environment files.

See [Environment variables](../project/env-vars.md) for Wasp's environment file rules.

### Preparing the database

#### SQLite

If you are using SQLite, it doesn't need a separate database process. It's enough to apply the migrations:

```bash
wasp db migrate-dev
```

#### PostgreSQL

If you are using PostgreSQL, it needs to be running. Start the Wasp dev database in one terminal:

```bash
wasp start db
```

Then apply the migrations from another terminal:

```bash
wasp db migrate-dev
```

Wasp gives each worktree a unique [development database](../data-model/databases.md#using-the-dev-database-provided-by-wasp) name and Docker volume.

#### Seed data

If your app needs to seed scaffold data, you can apply it with the `wasp db seed <name>` command.

### Start the app

After the setup, the app should start successfully:

```bash
wasp start
```

## Running worktrees at the same time

`wasp start` picks free ports for your app automatically, starting from `3000` for the client and `3001` for the server, so apps from different worktrees can run at the same time. Wasp derives your app's dev URLs from those ports and sets them for you, so overriding them in `.env.server` or `.env.client` fails. If you need specific ports, use [`--client-port` and `--server-port`](../general/cli.md#project-commands).

Wasp's dev PostgreSQL database still requires port `5432`, so to run apps in parallel worktrees, we need to manually provision a separate database for each worktree and set its `DATABASE_URL` in `.env.server`.

:::note
We are working on automatic port selection for [dev databases](https://github.com/wasp-lang/wasp/issues/4529).
:::

## Extra resources

### Using `.worktreeinclude` file

Some coding tools (e.g., [Claude Code](https://code.claude.com/docs/en/worktrees#copy-gitignored-files-into-worktrees), [Codex](https://learn.chatgpt.com/docs/environments/git-worktrees#copy-ignored-local-files-into-managed-worktrees), and [Conductor](https://www.conductor.build/docs/reference/worktreeinclude)) read a `.worktreeinclude` file and copy matching ignored files into new worktrees:

```gitignore title=".worktreeinclude"
.env.server
.env.client
```

Keep in mind that `.worktreeinclude` is not a Git feature, and support differs between tools.
