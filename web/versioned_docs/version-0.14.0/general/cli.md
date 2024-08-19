---
title: CLI Reference
---
This guide provides an overview of the Wasp CLI commands, arguments, and options.

## Overview

Once [installed](../quick-start), you can use the wasp command from your command line.

If you run the `wasp` command without any arguments, it will show you a list of available commands and their descriptions:

```
USAGE
  wasp <command> [command-args]

COMMANDS
  GENERAL
    new [<name>] [args]   Creates a new Wasp project. Run it without arguments for interactive mode.
      OPTIONS:
        -t|--template <template-name>
           Check out the templates list here: https://github.com/wasp-lang/starters

    new:ai <app-name> <app-description> [<config-json>]
      Uses AI to create a new Wasp project just based on the app name and the description.
      You can do the same thing with `wasp new` interactively.
      Run `wasp new:ai` for more info.

    version               Prints current version of CLI.
    waspls                Run Wasp Language Server. Add --help to get more info.
    completion            Prints help on bash completion.
    uninstall             Removes Wasp from your system.
  IN PROJECT
    start                 Runs Wasp app in development mode, watching for file changes.
    start db              Starts managed development database for you.
    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.
    clean                 Deletes all generated code, all cached artifacts, and the node_modules dir.
                          Wasp equivalent of 'have you tried closing and opening it again?'.
    build                 Generates full web app code, ready for deployment. Use when deploying or ejecting.
    deploy                Deploys your Wasp app to cloud hosting providers.
    telemetry             Prints telemetry status.
    deps                  Prints the dependencies that Wasp uses in your project.
    dockerfile            Prints the contents of the Wasp generated Dockerfile.
    info                  Prints basic information about the current Wasp project.
    test                  Executes tests in your project.
    studio                (experimental) GUI for inspecting your Wasp app.

EXAMPLES
  wasp new MyApp
  wasp start
  wasp db migrate-dev

Docs: https://wasp-lang.dev/docs
Discord (chat): https://discord.gg/rzdnErX
Newsletter: https://wasp-lang.dev/#signup
```

## Commands

### Creating a New Project
 - Use `wasp new` to start the interactive mode for setting up a new Wasp project.
 
  This will prompt you to input the project name and to select a template. The chosen template will then be used to generate the project directory with the specified name.

  ```
  $ wasp new
  Enter the project name (e.g. my-project) ▸ MyFirstProject
  Choose a starter template
  [1] basic (default)
      Simple starter template with a single page.
  [2] todo-ts
      Simple but well-rounded Wasp app implemented with Typescript & full-stack type safety.
  [3] saas
      Everything a SaaS needs! Comes with Auth, ChatGPT API, Tailwind, Stripe payments and more. Check out https://opensaas.sh/ for more details.
  [4] embeddings
      Comes with code for generating vector embeddings and performing vector similarity search.
  [5] ai-generated
      🤖 Describe an app in a couple of sentences and have Wasp AI generate initial code for you. (experimental)
  ▸ 1

  🐝 --- Creating your project from the "basic" template... -------------------------

  Created new Wasp app in ./MyFirstProject directory!

  To run your new app, do:
      cd MyFirstProject
      wasp db start
  ```
 - To skip the interactive mode and create a new Wasp project with the default template, use `wasp new <project-name>`.

   ```
   $ wasp new MyFirstProject
  🐝 --- Creating your project from the "basic" template... -------------------------

  Created new Wasp app in ./MyFirstProject directory!

  To run your new app, do:
      cd MyFirstProject
      wasp db start
   ```
### Project Commands
 - `wasp start` launches the Wasp app in development mode. It automatically opens a browser tab with your application running and watches for any changes to .wasp or files in `src/` to automatically reflect in the browser. It also shows messages from the web app, the server and the database on stdout/stderr.
 - `wasp start db` starts the database for you. This can be very handy since you don't need to spin up your own database or provide its connection URL to the Wasp app.
 - `wasp clean` removes all generated code and other cached artifacts. If using SQlite, it also deletes the SQlite database. Think of this as the Wasp version of the classic "turn it off and on again" solution.

    ```
    $ wasp clean

    🐝 --- Deleting the .wasp/ directory... -------------------------------------------

    ✅ --- Deleted the .wasp/ directory. ----------------------------------------------

    🐝 --- Deleting the node_modules/ directory... ------------------------------------

    ✅ --- Deleted the node_modules/ directory. ---------------------------------------
    ```

 - `wasp build` generates the complete web app code, which is ready for [deployment](../advanced/deployment/overview). Use this command when you're deploying or ejecting. The generated code is stored in the `.wasp/build` folder.

 - `wasp deploy` makes it easy to get your app hosted on the web.
 
  Currently, Wasp offers support for [Fly.io](https://fly.io). If you prefer a different hosting provider, feel free to let us know on Discord or submit a PR by updating [this TypeScript app](https://github.com/wasp-lang/wasp/tree/main/waspc/packages/deploy).
  
  Read more about automatic deployment [here](../advanced/deployment/cli).

 - `wasp telemetry` displays the status of [telemetry](https://wasp-lang.dev/docs/telemetry).

   ```
   $ wasp telemetry

   Telemetry is currently: ENABLED
   Telemetry cache directory: /home/user/.cache/wasp/telemetry/
   Last time telemetry data was sent for this project: 2021-05-27 09:21:16.79537226 UTC
   Our telemetry is anonymized and very limited in its scope: check https://wasp-lang.dev/docs/telemetry for more details.

   ```
 - `wasp deps` lists the dependencies that Wasp uses in your project.
 - `wasp info` provides basic details about the current Wasp project.
 - `wasp studio` shows you an graphical overview of your application in a graph: pages, queries, actions, data model etc.

### Database Commands
Wasp provides a suite of commands for managing the database. These commands all begin with `db` and primarily execute Prisma commands behind the scenes.

 - `wasp db migrate-dev` synchronizes the development database with the current state of the schema (entities). If there are any changes in the schema, it generates a new migration and applies any pending migrations to the database.
   - The `--name foo` option allows you to specify a name for the migration, while the `--create-only` option lets you create an empty migration without applying it.

 - `wasp db studio` opens the GUI for inspecting your database.

:::caution using `prisma` CLI directly

Although Wasp uses the `schema.prisma` file to define the database schema, you must not use the `prisma` command directly. Instead, use the `wasp db` commands.

Wasp adds some additional functionality on top of Prisma, and using `prisma` commands directly can lead to unexpected behavior e.g. missing auth models, incorrect database setup, etc.

:::

### Bash Completion

To set up Bash completion, run the `wasp completion` command and follow the instructions.


### Miscellaneous Commands 
 - `wasp version` displays the current version of the CLI.

    ```
    $ wasp version

    0.14.0

    If you wish to install/switch to the latest version of Wasp, do:
    curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s

    If you want specific x.y.z version of Wasp, do:
    curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z

    Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one.
    ```
 - `wasp uninstall` removes Wasp from your system.

   ```
   $ wasp uninstall

   🐝 --- Uninstalling Wasp ... ------------------------------------------------------

    We will remove the following directories:
      {home}/.local/share/wasp-lang/
      {home}/.cache/wasp/

    We will also remove the following files:
      {home}/.local/bin/wasp

    Are you sure you want to continue? [y/N]
    y

    ✅ --- Uninstalled Wasp -----------------------------------------------------------
   ```
