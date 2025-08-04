# Tutorial App Generator

CLI tool that generates tutorial apps by parsing markdown files and applying patches.

## Setup

```bash
npm install
```

## Usage

### Generate Tutorial App

Parses tutorial markdown files and generates a complete app with all steps applied:

```bash
npm run generate-app
```

This will:

1. Create a new Wasp app in `./TodoApp`
1. Initialize git repo with tagged commits
1. Parse tutorial files from `../docs/tutorial`
1. Apply patches for each tutorial step

### Edit Tutorial Step

Edit an existing tutorial step and regenerate patches:

```bash
npm run edit-step --step-id <id>
```

This will:

1. Switch to the step's git tag
1. Open interactive edit session
1. Update the patch file
1. Rebase subsequent commits

Make sure you first run the `generate-app` command to create the initial app structure.

## File Structure

- `src/extractSteps/` - Parses markdown files for `<TutorialAction>` components
- `src/executeSteps/` - Applies patches and runs migrations
- `src/commands/` - CLI command implementations
- `../docs/tutorial` - Generated patch files for each tutorial step

## Tutorial Markdown Format

Use `<TutorialAction>` components in markdown:

```jsx
<TutorialAction id="create-user" action="apply-patch" />
<TutorialAction id="setup-db" action="migrate-db" />
```

Supported actions:

- `apply-patch` - Applies a git patch file
- `migrate-db`
