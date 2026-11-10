---
title: "Quick start: install Wasp and create your first app"
sidebar_label: Quick start
description: Create a Wasp app with npx @wasp/cli new and run it with npm run dev. Then add the Wasp agent plugin to Claude Code or another coding agent.
slug: /quick-start
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import Trackable from '@site/src/components/Trackable';

# Quick start

## Installation

Welcome, new Waspeteer 🐝!

Let's create and run our first Wasp app in 2 short steps:

1. **To create a new app on Linux / macOS / WSL (Windows), open your terminal and run:**

   <Trackable event="CLI Command: Copy" command="wasp_new">

   <WaspCommand npm="npx @wasp/cli@latest new" global="wasp new" />

   </Trackable>

   ℹ️ Wasp requires Node.js and npm, which are usually installed together: check below for [more details](#requirements).

2. **Then, run the app:**

   <Trackable event="CLI Command: Copy" command="wasp_start">

   <WaspCommand npm={"cd <my-project-name>\nnpm run dev"} global={"cd <my-project-name>\nwasp start"} />

   </Trackable>

That's it 🎉 You have successfully created a new full-stack web app and Wasp is serving both frontend and backend for you. Open the client URL that Wasp printed on startup (usually [http://localhost:3000](http://localhost:3000), unless it's taken) to see it.

:::tip Prefer shorter commands?
These commands run the Wasp your project installed. To type `wasp start` instead, install the [global Wasp CLI](#global-cli).
:::

But don't stop there! Turn your coding agent into a Wasp framework expert in the next step.

3. **Install Agent Plugin / Skills:**

   **Claude Code:**

    <Trackable event="Agent Plugin: Copy" kind="claude_plugin">

    ```bash
    claude plugin marketplace add wasp-lang/wasp-agent-plugins
    claude plugin install wasp@wasp-agent-plugins --scope project
    ```

    </Trackable>

   **Other Agents (Cursor, Codex, Gemini, Copilot, OpenCode, etc.):**

    <Trackable event="Agent Plugin: Copy" kind="skills_cli">

    <PackageManagerCommand npm="npx skills add wasp-lang/wasp-agent-plugins" />

    </Trackable>

    **Initialize the plugin / skills:**
    Invoke the `/wasp-plugin-init` skill to add Wasp knowledge to your agent's memory file (e.g. `CLAUDE.md`, `AGENTS.md`):

    <Trackable event="Agent Plugin: Copy" kind="plugin_prompt">

    ```bash
    Run the '/wasp-plugin-init' skill.
    ```

    </Trackable>

   For more info check out the [Wasp Agent Plugin / Skills](./agent-integration.md) page.

:::note Something Unclear?
Check [More Details](#more-details) section below if anything went wrong with the installation, or if you have additional questions.
:::

:::tip Having trouble running Wasp?
  If you get stuck with a weird error while developing with Wasp, try running `wasp clean` - this is the Wasp equivalent of "turning it off and on again"!
  Do however let us know about the issue on our GitHub repo or Discord server.
:::

### What next?

- [ ] 👉 **Check out the [Todo App tutorial](../tutorial/01-create.md), which will take you through all the core features of Wasp!** 👈
- [ ] Join us on [Discord](https://discord.gg/rzdnErX)! Any feedback or questions you have, we are there for you.
- [ ] Follow Wasp development by subscribing to our newsletter: https://wasp.sh/#signup . We usually send 1 per month, and [Matija](https://github.com/matijaSos) does his best to unleash his creativity to make them engaging and fun to read :D!

---

## More details

### Requirements

You must have Node.js (and NPM) installed on your machine and available in `PATH`.
<!--
  NOTE: Don't change Wasp's lowest supported Node version without updating it in
  all required places. Check /.node-version for the full list.
-->
A version of Node.js must be >= 24.22.1.

If you need it, we recommend using [nvm](https://github.com/nvm-sh/nvm) for managing your Node.js installation version(s).

<details>
  <summary style={{cursor: 'pointer', 'textDecoration': 'underline'}}>
    A quick guide on installing/using nvm
  </summary>

  <div>
    Install nvm via your OS package manager (`apt`, `pacman`, `homebrew`, ...) or via the [nvm](https://github.com/nvm-sh/nvm#install--update-script) install script.

    Then, install a version of Node.js that you need:

    ```shell
    nvm install 24
    ```

    Finally, whenever you need to ensure a specific version of Node.js is used, run:

    ```shell
    nvm use 24
    ```

    to set the Node.js version for the current shell session.

    You can run

    ```shell
    node -v
    ```

    to check the version of Node.js currently being used in this shell session.

    Check NVM repo for more details: https://github.com/nvm-sh/nvm.
  </div>
</details>

### Installation {#detailed-installation}

Wasp installs as an npm dependency of each project. `wasp new` adds `@wasp/wasp` to the new project's `package.json` and runs `npm install`, which installs Wasp along with the rest of the project's dependencies. After you clone a project, run `npm install` yourself.

The `@wasp/wasp` version in `package.json` is the project's Wasp version, so everyone working on the project uses the same Wasp. See [Running Wasp commands](../advanced/cli.md#running-wasp-commands) for how to run it.

#### Installing Wasp globally {#global-cli}

You don't need a global install to use Wasp. If you prefer shorter commands, and want the Wasp CLI available anywhere, even outside of projects, install it globally:

<Trackable event="Install Command: Copy">

<PackageManagerCommand npm="npm i -g @wasp/cli@latest" />

</Trackable>

Inside a project, the global CLI hands each command over to the project's own Wasp, the `@wasp/wasp` version from its `package.json`. So the version of the global CLI never conflicts with the project's Wasp version, and each of your projects can use a different one.

Outside of a project, there's no project Wasp to hand the command over to, so the global CLI runs it with its own version. For example, `wasp new` creates projects on the global CLI's version. You can update the global CLI at any time to run these commands with the latest Wasp.

:::note Looking for the old installer script?

Check out the **[Legacy installer guide](../guides/legacy/installer.md)** for instructions on how to migrate to npm-based installation.

:::

#### Upgrading Wasp

To switch a project to another Wasp version, change its `@wasp/wasp` dependency:

<PackageManagerCommand npm="npm install --save-exact @wasp/wasp@x.y.z" />

Check the [migration guide](../migration-guide.md) before upgrading to a new minor version.

#### Windows and building from source

<Tabs
  defaultValue="win"
  values={[
    {label: 'Windows', value: 'win'},
    {label: 'From source', value: 'source'}
  ]}
>
  <TabItem value="win">
    With Wasp for Windows, we are almost there: Wasp is successfully compiling and running on Windows but there is a bug or two stopping it from fully working. Check it out [here](https://github.com/wasp-lang/wasp/issues/48) if you are interested in helping.

    In the meantime, the best way to start using Wasp on Windows is by using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install). Once you set up Ubuntu on WSL, just follow Linux instructions for installing Wasp. You can refer to this [article](https://wasp.sh/blog/2023/11/21/guide-windows-development-wasp-wsl) if you prefer a step by step guide to using Wasp in WSL environment. If you need further help, reach out to us on [Discord](https://discord.gg/rzdnErX) - we have some community members using WSL that might be able to help you.
    :::caution
    If you are using WSL2, make sure that your Wasp project is not on the Windows file system, but instead on the Linux file system. Otherwise, Wasp won't be able to detect file changes, due to the [issue in WSL2](https://github.com/microsoft/WSL/issues/4739).
    If you use VS Code, install the [WSL extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) and open the project from inside WSL (run `code .` in the project directory). Without it, TypeScript support in VS Code won't work on WSL2.
    :::
  </TabItem>

  <TabItem value="source">
    If the other methods are not working for you or your OS is not supported, you can try building Wasp from the source.

    To build from source, you need to clone the [wasp repo](https://github.com/wasp-lang/wasp), install [mise](https://mise.jdx.dev/getting-started.html) on your machine and then run these commands from the repo:

    ```shell
    mise install
    mise run build
    ```

    You can then run your build with `mise run wasp-cli`, or run `mise run install` to install it globally as `wasp-cli`.

    If you have never built Wasp before, this might take some time due to `cabal` downloading dependencies for the first time.

    Check [waspc/](https://github.com/wasp-lang/wasp/tree/main/waspc) for more details on building Wasp from the source.
  </TabItem>
</Tabs>
