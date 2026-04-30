---
title: Agent Plugin / Skills
---

Wasp provides an official plugin for coding agents that transforms them into Wasp framework experts. 

The plugin gives your agent curated access to Wasp docs, workflows, and best practices so it can develop full-stack web apps (React, Node.js, Prisma) more effectively.

The plugin / skills work with just about all the popular coding agents, such as [Claude Code](https://claude.com/product/claude-code), [Cursor](https://www.cursor.com/), [Codex](https://openai.com/codex/), [Gemini CLI](https://geminicli.com/), [GitHub Copilot](https://github.com/features/copilot/cli), [OpenCode](https://opencode.ai/), and more.

## Features

- **Wasp Documentation** — Ensures your agent always accesses LLM-friendly Wasp docs in sync with your current project's Wasp version.
- **Wasp Knowledge** — Imports Wasp best practices and conventions into your project's memory file (e.g. `CLAUDE.md`, `AGENTS.md`).
- **Feature Configuration** — Easily add Wasp features like authentication, database, email, styling (Tailwind, shadcn/ui), and other integrations through your agent.
- **Deployment Guidance** — Your agent will guide you through deploying your Wasp app to Railway or Fly.io via the Wasp CLI, or manually to your favorite cloud provider.

## Installation

### Claude Code

First, add the Wasp plugin marketplace:

```bash
claude plugin marketplace add wasp-lang/wasp-agent-plugins
```

Then install the Wasp plugin:

```bash
claude plugin install wasp@wasp-agent-plugins --scope project
```

:::tip
We recommend installing with `project` scope so the settings are committed to git (via `settings.json`). Use `local` scope if you prefer settings that aren't committed (via `settings.local.json`).
:::

### Other Agents (Cursor, Codex, Gemini, Copilot, OpenCode, etc.)

Run the following command and select all the skills:

```bash
npx skills add wasp-lang/wasp-agent-plugins
```

## Setup

After installing, initialize the plugin in an active agent session by explicitly invoking the `/wasp-plugin-init` skill:

```
Run the '/wasp-plugin-init' skill.
```

This adds Wasp knowledge to your project's `CLAUDE.md` or `AGENTS.md` file.

Next, start the development server as a background task so your agent has full insight into the running app while developing:

```
Run the 'start-dev-server' skill.
```

To see all available features and skills:

```
/wasp-plugin-help
```

## Learn More

Check out the [Wasp Agent Plugins repository](https://github.com/wasp-lang/wasp-agent-plugins) for more details.
