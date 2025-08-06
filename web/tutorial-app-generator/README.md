# Tutorial Steps Executor (tutse)

A CLI tool that generates Wasp tutorial applications by parsing markdown files containing tutorial steps and automatically applying patches and database migrations.

## Overview

This tool reads tutorial markdown files, extracts `<TutorialAction>` components, and executes the corresponding actions to build a complete tutorial app step by step. Each step is tracked with git commits and tags for easy navigation and editing.

## Setup

```bash
npm install
```

## Commands

### Generate Tutorial App

Generates a complete tutorial app by executing all tutorial steps in sequence:

```bash
npm run generate-app
```

This command:
1. Creates a new Wasp app using `wasp new`
2. Initializes a git repository with proper branching
3. Parses all tutorial markdown files from the tutorial directory
4. Executes actions (patches, migrations) in order
5. Creates git tags for each completed step
6. Outputs the final app in the configured app directory

### List Tutorial Steps

Display all available tutorial steps organized by source file:

```bash
npm run list-steps
```

### Edit Tutorial Step

Edit a specific tutorial step and regenerate associated patches:

```bash
npm run edit-step --step-id <step-id>
# or
npm run edit-step  # Interactive step selection
```

Options:
- `--step-id <id>`: Specify the step ID to edit
- `--skip-generating-app`: Skip app generation and use existing app

This command:
1. Generates the app (if not skipped)
2. Switches to the step's git tag
3. Creates a `fixes` branch for editing
4. Opens a new shell in the app directory, allowing you to make code changes.
5. Once you exit the shell, it updates the patch file with your changes.
6. Rebases subsequent commits to incorporate changes

## Tutorial Markdown Format

Tutorial steps are defined using `<TutorialAction>` JSX components within markdown files:

```jsx
<TutorialAction id="unique-step-id" action="apply-patch" />
<TutorialAction id="setup-database" action="migrate-db" />
```

### Required Attributes

- `id`: Unique identifier for the step (used for git tags and patch file names)
- `action`: Type of action to execute

### Supported Actions

#### `apply-patch`
Applies a git patch file to modify the codebase:
- Looks for patch file at `patches/{step-id}.patch`
- Automatically attempts to fix failed patches
- Creates git commit with the changes

#### `migrate-db`
Runs database migration using Wasp CLI, executes `wasp db migrate-dev`.

## Tutorial File Organization

Tutorial markdown files should be:
- Located in the configured tutorial directory
- Named with numeric prefixes for ordering (e.g., `01-setup.md`, `02-auth.md`)
- Contain `<TutorialAction>` components for executable steps

Example tutorial file structure:
```
tutorial/
├── 01-getting-started.md
├── 02-database-setup.md
├── 03-authentication.md
└── 04-frontend-features.md
```

## Configuration

The tool uses configuration from `src/project.ts`:
- Tutorial directory path
- App output directory
- Patches directory
- Git branch naming

## Git Integration

Each tutorial step creates:
- A git commit with the step changes
- A git tag with the step ID for easy navigation
- Proper branch management for editing workflows

### Rebasing on Edit

When you edit a tutorial step using `npm run edit-step`, the tool performs a git rebase to update all subsequent steps. Here's how it works:

1.  The tool checks out the commit just before the one you want to edit.
2.  You make your changes in a temporary `fixes` branch.
3.  A new patch is generated from your changes, and a new commit is created for the edited step.
4.  The tool then automatically rebases all subsequent tutorial steps on top of this new commit. This ensures that your changes are propagated through the rest of the tutorial, and all patches and tags are updated accordingly.
