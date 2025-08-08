# Tutorial Steps Executor

A CLI tool for managing and editing tutorial steps in Wasp docs. This tool allows you to execute all tutorial steps to get the final app or edit individual steps.

## Comamnds

The CLI provides three main commands:

### 1. Generate App (`npm run generate-app`)

Creates a complete Wasp application by executing all tutorial steps in sequence.

```bash
npm run generate-app
```

This command:

- Initializes a new Wasp application
- Reads all tutorial markdown files (numbered like `01-setup.md`, `02-auth.md`, etc.)
- Extracts `<TutorialAction>` components from the markdown
- Applies each step's patches or database migrations in order
- Creates a Git commit for each step
- Results in a fully functional application with complete Git history

### 2. Edit Step (`npm run edit-step`)

Allows you to modify a specific tutorial step and automatically update all subsequent steps.

```bash
npm run edit-step --step-id "create-task-entity"
# or interactive mode:
npm run edit-step
```

This command:

- Generates the app (unless `--skip-generating-app` is used)
- Creates a branch from the step's commit
- Captures your changes as a new patch
- Rebases the changes through all subsequent steps
- Updates all affected patch files

### 3. List Steps (`npm run list-steps`)

Displays all available tutorial steps organized by source file.

```bash
npm run list-steps
```

## How It Works on Git Level, Patches, and Rebasing

The tool uses a Git-based workflow to manage tutorial steps:

### Executing Tutorial Steps

1. **Initial Setup**: Creates a Git repository with an initial commit
2. **Step Execution**: Each step is executed and commited as a separate Git commit
   with the step ID as the commit message

### Step Editing Process

When editing a tutorial step (e.g., step 4 out of 10 total steps):

#### Phase 1: Setup and Branching

```bash
# Generate app with all 10 steps, each as a commit
git init
git commit -m "Initial commit"
git commit -m "step-1-setup"
git commit -m "step-2-auth"
git commit -m "step-3-database"
git commit -m "step-4-create-task-entity"  # ← Target step
# ... and so on

# Create branch from step 4's commit
git switch --force-create fixes <step-4-commit-sha>
```

#### Phase 2: User Editing

```bash
# Move the step 4 commit changes to staging area
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

# Apply the new patch and commit with original step ID
git apply new-patch.patch
git commit -m "step-4-create-task-entity"
```

#### Phase 4: Rebasing and Integration

```bash
# Switch back to main branch and rebase the fixes
git switch main
git rebase fixes # This integrates the fixed step 4

# If conflicts occur, user resolves them like any other Git conflict
```

#### Phase 5: Patch File Updates

```bash
# Regenerate all patch files from the updated commits
# For each step after the edited one:
git show step-commit-sha --format= > patches/step-N.patch
```

### Patch File Management

- Patch files are stored in the `./docs/tutorial/patches/` directory
- Files are named after their step IDs (e.g., `create-task-entity.patch`)
- Contains the Git diff for that specific step
- When a step is edited, all subsequent patch files are automatically regenerated

### Database Migration Handling

For steps that involve database changes:

- Uses `wasp db migrate` command instead of patch application
- Migration files are committed with the step ID as the commit message

### Tutorial File Format

Tutorial steps are defined in markdown files using JSX-like components:

```markdown
# Step 4: Create Task Entity

<TutorialAction id="create-task-entity" action="apply-patch" />

In this step, we'll create a Task entity...
```

The tool extracts these components and uses:

- `id`: Unique identifier for the step (becomes commit message)
- `action`: Type of action (`apply-patch` or `migrate-db`)

This Git-based approach ensures that:

- **Changes can be made to any step** without breaking subsequent steps
- **Conflicts are automatically handled** by Git's rebasing mechanism
