# Wasp Starters

This directory contains starter templates for creating new Wasp projects.

## How Wasp Starters Work

When you create a new project using `wasp new`, Wasp creates the project in 3 steps:

1. **Copy skeleton files**: Wasp copies the [`skeleton`](./skeleton) files into the project directory
2. **Copy starter template files**: Wasp copies the chosen starter template files (e.g., `basic`) into the project directory
3. **Replace placeholders**: Wasp replaces template placeholders in the project files (e.g., app name)

Due to this behavior, all files shared by all starters are placed in the [`skeleton`](./skeleton) directory.

## Development Workflow

The recommended approach for developing starter templates:

1. **Create a test project**: Create a new project with the starter you want to modify
2. **Implement changes**: Make your required changes in the Wasp project
3. **Copy changes back**: Once satisfied with the implementation, copy the files from your project back to the starter templates
4. **Test the starter**: Create a new project with `wasp-cli` to verify it matches your previously created project
