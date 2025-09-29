# Tutorial Actions Executor (tacte)

## What is this CLI?

Wasp docs have a tutorial that walks users through building a complete Wasp application step-by-step.

Next to the text that explains each step, we added the `<TutorialAction>` components that define actions
that define machine executable steps, like "create a new Wasp app", "add authentication", "create a Task entity", etc.
This CLI tool, reads those tutorial files, extracts the actions, and executes them in sequence
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

- Initializes a new Wasp application
- Reads all tutorial files (numbered like `01-setup.md`, `02-auth.md`, etc.)
- Extracts `<TutorialAction>` components from the file
- Applies each action's patches or database migrations in order
- Creates a Git commit for each action
- Results in a fully functional application

If one of the patches fails to apply, the CLI will ask you to recreate the patch manually
and try to continue.

### 2. Edit Action (`npm run edit-action`)

Allows you to modify a specific patch action and automatically update all subsequent patch actions.

```bash
# Non-interactive (direct by ID):
npm run edit-action -- --action-id "create-task-entity"

# Interactive (pick an action from a list):
npm run edit-action

# Optional flags:
# - skip generating app before editing
npm run edit-action -- --skip-generating-app
# - pass a custom Wasp CLI
npm run edit-action -- --wasp-cli-command wasp
```

This command:

- Applies all actions before the target action
- Moves all the changes from the target action to the staging area
- Allows you to edit the code in your editor
- Updates the patch based on your changes
- Reapplies all subsequent actions, allowing you to resolve any conflicts

### 3. List Actions (`npm run list-actions`)

Displays all available tutorial actions organized by source file.

```bash
npm run list-actions
```

You will see actions grouped by tutorial filename, including the action `id` and its `kind`.

### Patch File Management

- Patch files are stored in the `./docs/tutorial/patches/` directory
- Files are named using the source file and the action id
- Each patch file contains a Git diff for that specific action

### Tutorial File Format

Tutorial actions are defined in MDX files using JSX components:

````markdown
# Step 4: Create Task Entity

<TutorialAction id="create-task-entity" action="APPLY_PATCH" />

In this action, we'll create the Task entity:

```prisma
model Task {
   id        Int      @id @default(autoincrement())
}
```
````

The tool extracts these components and uses:

- `id`: Unique identifier for the action (becomes commit message)
- `action`: Type of action (`INIT_APP`, `APPLY_PATCH`, `MIGRATE_DB`)

This Git-based approach ensures that:

- **Changes can be made to any action** without breaking subsequent actions
- **Conflicts are automatically handled** by Git's rebasing mechanism

## Extra Info: How It Works on Git Level, Patches, and Rebasing

The tool uses a Git-based workflow to manage tutorial actions:

### Executing Tutorial Actions

1. **Initial Setup**: Creates a Git repository with an initial commit
2. **Action Execution**: Each action is executed and committed as a separate Git commit
   with the action ID as the commit message

### Action Editing Process

When editing a tutorial action (e.g., action 4 out of 10 total actions):

#### Phase 1: Setup and Branching

```bash
# Generate app with all 10 actions, each as a commit
git init
git commit -m "Initial commit"
git commit -m "action-1-setup"
git commit -m "action-2-auth"
git commit -m "action-3-database"
git commit -m "action-4-create-task-entity"  # ← Target action
# ... and so on

# Create branch from action 4's commit
git switch --force-create fixes <action-4-commit-sha>
```

#### Phase 2: User Editing

```bash
# Move the action 4 commit changes to staging area
git reset --soft HEAD~1

# User makes their edits in the editor
# User confirms they're done
```

#### Phase 3: Patch Creation and Application

```bash
# Commit all current changes and generate a new patch
git add .
git commit -m "temporary-commit"
git show HEAD --format= > new-patch.patch
git reset --hard HEAD~1

# Apply the new patch and commit with original action ID
git apply new-patch.patch
git commit -m "action-4-create-task-entity"
```

#### Phase 4: Rebasing and Integration

```bash
# Switch back to main branch and rebase the fixes
git switch main
git rebase fixes # This integrates the fixed action 4

# If conflicts occur, user resolves them like any other Git conflict
```

#### Phase 5: Patch File Updates

```bash
# Regenerate all patch files from the updated commits
# For each action after the edited one:
git show action-commit-sha --format= > patches/action-N.patch
```
