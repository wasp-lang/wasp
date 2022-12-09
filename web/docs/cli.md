---
title: CLI Reference
---
This document describes the Wasp CLI commands, arguments, and options.

## Overview

The `wasp` command can be called from command line once [installed](https://wasp-lang.dev/docs/#2-installation). 
When called without arguments, it will display its command usage and help document:

```
USAGE
  wasp <command> [command-args]

COMMANDS
  GENERAL
    new <project-name>    Creates new Wasp project.
    version               Prints current version of CLI.
    waspls                Run Wasp Language Server. Add --help to get more info.
    completion            Prints help on bash completion.
  IN PROJECT
    start                 Runs Wasp app in development mode, watching for file changes.
    db <db-cmd> [args]    Executes a database command. Run 'wasp db' for more info.
    clean                 Deletes all generated code and other cached artifacts. Wasp equivalent of 'have you tried closing and opening it again?'.
    build                 Generates full web app code, ready for deployment. Use when deploying or ejecting.
    telemetry             Prints telemetry status.
    deps                  Prints the dependencies that Wasp uses in your project.
    dockerfile            Prints the contents of the Wasp generated Dockerfile.
    info                  Prints basic information about current Wasp project.

EXAMPLES
  wasp new MyApp
  wasp start
  wasp db migrate-dev

Docs: https://wasp-lang.dev/docs
Discord (chat): https://discord.gg/rzdnErX
Newsletter: https://wasp-lang.dev/#signup
```

## Commands
### General
 - `wasp new <project-name>` creates new Wasp project. A directory with the provided project-name will be created, containing boilerplate code.
 
   ```
   $ wasp new MyFirstProject
   ```
 - `wasp version` prints current version of CLI.
 
   ```
   $ wasp version
   
   0.2.0.1
   ``` 

### Bash Completion

To setup Bash completion, execute `wasp completion` and follow the instructions.

### In project
 - `wasp start` runs Wasp app in development mode. It opens a browser tab with your application running, and watches for any changes to .wasp or files in `src/` to automatically reflect in the browser. It also shows messages from the web app, the server and the database on stdout/stderr.
 
 - `wasp clean` deletes all generated code and other cached artifacts. If using SQlite, it also deletes the SQlite database. It is the Wasp equivalent to "try shutting it down and turning back on".
  
   ```
   $ wasp clean
   
   Deleting .wasp/ directory...
   Deleted .wasp/ directory.
   ```
 
 - `wasp build` generates full web app code, ready for deployment. Use when deploying or ejecting. Generated code goes in the .wasp/build folder.
  
 - `wasp telemetry` prints [telemetry](https://wasp-lang.dev/docs/telemetry) status.
   
   ```
   $ wasp telemetry 
   
   Telemetry is currently: ENABLED
   Telemetry cache directory: /home/user/.cache/wasp/telemetry/
   Last time telemetry data was sent for this project: 2021-05-27 09:21:16.79537226 UTC
   Our telemetry is anonymized and very limited in its scope: check https://wasp-lang.dev/docs/telemetry for more details.

   ```
 - `wasp deps` prints the dependencies that Wasp uses in your project.
 - `wasp info` prints basic information about current Wasp project.

   
#### Database 
Wasp has a set of commands for working with the database. They all start with `db` and mostly call prisma commands in the background.

 - `wasp db migrate-dev` ensures dev database corresponds to the current state of schema (entities): it generates a new migration if there are changes in the schema and it applies any pending migration to the database.
   - Supports a `--name foo` option for providing a migration name, as well as `--create-only` for creating an empty migration but not applying it.
   
 - `wasp db studio` opens the GUI for inspecting your database.
