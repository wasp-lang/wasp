# Tutorial Actions Executor (tacte)

A CLI tool for managing and editing tutorial actions in Wasp docs. This tool allows you to execute all tutorial actions to get the final app or edit individual actions.

## Comamnds

The CLI provides three main commands:

### 1. Generate App (`npm run generate-app`)

Creates a complete Wasp application by executing all tutorial actions in sequence.

```bash
npm run generate-app
```

This command:

- Initializes a new Wasp application
- Reads all tutorial markdown files (numbered like `01-setup.md`, `02-auth.md`, etc.)
- Extracts `<TutorialAction>` components from the markdown
- Applies each action's patches or database migrations in order
- Creates a Git commit for each action
- Results in a fully functional application.

### 2. Edit Action (`npm run edit-action`)

Allows you to modify a specific tutorial action and automatically update all subsequent actions.

```bash
npm run edit-action --action-id "create-task-entity"
# or interactive mode:
npm run edit-action
```

This command:

- Generates the app (unless `--skip-generating-app` is used)
- Creates a branch from the action's commit
- Captures your changes as a new patch
- Updates all affected patch files

### 3. List Actions (`npm run list-actions`)

Displays all available tutorial actions organized by source file.

```bash
npm run list-actions
```


### Patch File Management

- Patch files are stored in the `./docs/tutorial/patches/` directory
- Files are named after their action IDs (e.g., `create-task-entity.patch`)
- Contains the Git diff for that specific action
- When a action is edited, all subsequent patch files are automatically regenerated

### Database Migration Handling

For actions that involve database changes:

- Uses `wasp db migrate` command instead of patch application
- Migration files are committed with the action ID as the commit message

### Tutorial File Format

Tutorial actions are defined in markdown files using JSX-like components:

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
- `action`: Type of action (`APPLY_PATCH` or `MIGRATE_DB`)

This Git-based approach ensures that:

- **Changes can be made to any action** without breaking subsequent actions
- **Conflicts are automatically handled** by Git's rebasing mechanism

## How It Works on Git Level, Patches, and Rebasing

The tool uses a Git-based workflow to manage tutorial actions:

### Executing Tutorial Actions

1. **Initial Setup**: Creates a Git repository with an initial commit
2. **Action Execution**: Each action is executed and commited as a separate Git commit
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