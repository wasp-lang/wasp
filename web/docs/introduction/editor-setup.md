---
title: Editor Setup
slug: /editor-setup
---

:::note
This page assumes you have already installed Wasp. If you do not have Wasp installed yet, check out the [Quick Start](./quick-start.md) guide.
:::

Wasp Spec files are TypeScript files, so editor support comes from your editor's regular TypeScript tooling.

## TypeScript support

Use any editor with TypeScript language service support. In Wasp projects, this gives you:

- type checking and diagnostics for `main.wasp.ts`
- autocompletion for `@wasp.sh/spec` constructors and options
- go to definition for [reference imports](../general/spec.md#reference-imports)
- import path checks for files in `src`

For `schema.prisma`, install the [Prisma extension for VS Code](https://marketplace.visualstudio.com/items?itemName=Prisma.prisma) or the equivalent Prisma extension for your editor.

If your editor reports stale type or import errors after changing Wasp files, restart the TypeScript server. In VS Code, open the command palette and run _"TypeScript: Restart TS Server."_
