# Tutorial Actions Executor (tacte)

## What is this CLI?

Wasp docs have a tutorial that walks users through building a complete Wasp application step-by-step.

Next to the text that explains each step, we added `<TutorialAction>` components that define machine-executable steps,
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

- Reads all tutorial files (numbered like `01-setup.md`, `02-auth.md`, etc.)
- Extracts `<TutorialAction>` components from the file
- Applies each action in sequence (e.g. initialize app, apply patches, migrate DB)
- Results in a fully functional application

One of the actions is to apply a Git patch that modifies a source file. If applying
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

- Executes all actions before the target action
- Moves all the changes from the target action to the Git staging area
- Allows you to edit the code in your editor
- Updates the patch based on your changes
- Reapplies all subsequent actions

### 3. List Actions (`npm run list-actions`)

Displays all available tutorial actions organized by source file.

```bash
npm run list-actions
```

Shows actions grouped by tutorial file, including each action's `id` and `kind`.

### Patch File Management

- Patch files are stored in `./docs/tutorial/patches/`
- Files are named using the source file and action ID
- Each patch file contains a Git diff for that specific action

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

- `id`: Unique identifier for the action (becomes commit message)
- `action`: Type of action (`INIT_APP`, `APPLY_PATCH`, `MIGRATE_DB`)
