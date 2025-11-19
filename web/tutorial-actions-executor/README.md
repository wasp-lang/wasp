# Tutorial Actions Executor (tacte)

## What is this CLI?

Wasp docs have a tutorial that walks users through building a complete Wasp application step-by-step.

Next to the text that explains each step, we added `<TutorialAction>` components that define machine-executable actions,
like "create a new Wasp app", "add authentication", "create a Task entity", etc.
This CLI tool reads those tutorial files, extracts the actions, and executes them in sequence
to create a fully functional Wasp application.

## Commands

The CLI provides three commands:

### 1. Generate App (`npm run generate-app`)

Creates a complete Wasp application by executing all tutorial actions in sequence.

```bash
npm run generate-app
# Optional: pass a custom Wasp CLI binary/command
npm run generate-app -- --wasp-cli-command wasp
```

This command:

- Reads all tutorial files (numbered like `01-setup.md`, `02-auth.md`, etc.).
- Extracts actions from `<TutorialAction>` components in each tutorial file.
- Applies each action in sequence (e.g. initialize app, apply patches, migrate DB).
- The result of applying all of the actions in a fully functional application.

One of the action types is to apply a Git patch that modifies a source file. If applying
a patch fails due to conflicts, `generate-app` command pauses and allows you
to resolve the conflicts manually.

### 2. Edit Patch Action (`npm run edit-patch-action`)

Allows you to modify a specific patch action and automatically reapplies all subsequent actions.

```bash
# Non-interactive (direct by ID):
npm run edit-patch-action -- --action-id "create-task-entity"

# Interactive (pick an action from a list):
npm run edit-patch-action

# Optional flags:
# - skip generating app before editing
npm run edit-patch-action -- --skip-generating-app
# - pass a custom Wasp CLI
npm run edit-patch-action -- --wasp-cli-command wasp
```

This command:

- Generates the app from scratch using generate-app command.
  - Generating the apps results in having each action as a separate commit in Git history.
- "Rewinds" the app to the commit of the specified action and allows you to make changes
  to its code as needed.
- Asks you to confirm when you are done, updates the commit of the action with the changes
  you did, and reapplies all subsequent commits on top of it.
  - There might be conflicts between your new changes and subsequent commits.
    If so, you will need to resolve them manually.

### 3. List Actions (`npm run list-actions`)

Displays all available tutorial actions organized by source file.

```bash
npm run list-actions
```

Shows actions grouped by tutorial file, including each action's `id` and `kind`.

### Required Command Options

All commands require the following options to set up the tutorial app configuration:

- `--app-name <name>`: Name of the app to generate.
- `--output-dir <path>`: Directory where the app will be generated (default: `./.result`)
- `--tutorial-dir <path>`: Directory containing the tutorial files.

For example:

```bash
npm run generate-app -- --app-name MyApp --output-dir ./custom-output --tutorial-dir ./my-tutorial
```

### Patch File Management

- Patch files need to exist in the `patches` dir in the configured tutorial dir
- Files are named using the source file and action ID.
- Each patch file contains a Git diff for that specific action.

### Tutorial File Format

Tutorial actions are defined in MDX files using JSX components:

````mdx
# Step 4: Create Task Entity

In this action, we'll create the Task entity:

<TutorialAction id="create-task-entity" action="APPLY_PATCH">
```prisma
model Task {
   id        Int      @id @default(autoincrement())
}
```
</TutorialAction>
````

The `<TutorialAction>` component should wrap the part of the tutorial text that it is associated with.

The tool extracts these components and uses:

- `id`: Unique identifier for the action (becomes commit message),
- `action`: Type of action (`INIT_APP`, `APPLY_PATCH`, `MIGRATE_DB`).

## Testing

The project includes both unit tests and end-to-end (e2e) snapshot tests.

You can run all tests using:

```bash
npm run test
```

### E2E Snapshot Tests

E2E tests verify the entire tutorial action execution flow using snapshot testing.

Snapshot testing captures the output of the tutorial action executor (generated files, git history, etc.)
and stores it as a "snapshot". On subsequent test runs, the output is compared against the stored snapshot
to ensure nothing has changed unexpectedly.

#### E2E Test Structure

- Test fixtures: `e2e-tests/fixtures/tutorial/` contains minimal tutorial files used for testing.
- Generated output: `e2e-tests/.result/`.
- Snapshots: `e2e-tests/__snapshots__/`.

#### Updating Snapshots

When you intentionally change the tutorial action executor behavior, run the tests in update mode:

```bash
$ npm run test --update
```
