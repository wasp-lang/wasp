---
title: Editor Setup
slug: /editor-setup
---

import TypescriptServerNote from '../_TypescriptServerNote.md'

:::note
This page assumes you have already installed Wasp. If you do not have Wasp installed yet, check out the [Quick Start](./quick-start.md) guide.
:::

Wasp comes with the Wasp language server, which gives supported editors powerful support and integration with the language.

## VSCode

Currently, Wasp only supports integration with VSCode. Install the [Wasp language extension](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) to get syntax highlighting and integration with the Wasp language server.

The extension enables:

- syntax highlighting for `.wasp` files
- the Prisma extension for `.prisma` files
- scaffolding of new project files
- code completion
- diagnostics (errors and warnings)
- go to definition

and more!

## Coding Agents

If you use a coding agent like Claude Code, Cursor, Codex, OpenCode, GitHub Copilot, etc., check out the [Wasp Coding Agent Plugin](../wasp-ai/coding-agent-plugin.md) to give your agent full Wasp framework knowledge.

<TypescriptServerNote />
