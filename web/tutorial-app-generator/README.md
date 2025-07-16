# Tutorial App Generator

Generates a Wasp app by executing tutorial steps from markdown files.

## Usage

```bash
npm run start
```

## Options

- `-s, --until-step <step>` - Run until the given step number
- `-e, --broken-diff <step>` - Edit mode for fixing diffs interactively

## Examples

```bash
# Run all steps
npm run start

# Run until step 5
npm run start -- -s 5

# Edit diff at step 3
npm run start -- -e 3
```

## What it does

1. Creates a new Wasp app in the output directory
2. Parses tutorial markdown files to extract steps
3. Executes diff actions and other steps sequentially
4. Supports interactive editing of broken diffs
