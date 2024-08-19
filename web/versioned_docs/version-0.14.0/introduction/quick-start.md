---
title: Quick Start
slug: /quick-start
---

import useBaseUrl from '@docusaurus/useBaseUrl';

## Installation

Welcome, new Waspeteer 🐝!

Let's create and run our first Wasp app in 3 short steps:

1. **To install Wasp on Linux / OSX / WSL (Windows), open your terminal and run:**

   ```shell
   curl -sSL https://get.wasp-lang.dev/installer.sh | sh
   ```

   ℹ️ Wasp requires Node.js and will warn you if it is missing: check below for [more details](#requirements).

2. **Then, create a new app by running:**

   ```shell
   wasp new
   ```

3. **Finally, run the app:**

   ```shell
   cd <my-project-name>
   wasp start
   ```

That's it 🎉 You have successfully created and served a new full-stack web app at <http://localhost:3000> and Wasp is serving both frontend and backend for you.

:::note Something Unclear?
  Check [More Details](#more-details) section below if anything went wrong with the installation, or if you have additional questions.
:::

:::tip Want an even faster start?
  Try out [Wasp AI](../wasp-ai/creating-new-app.md) 🤖 to generate a new Wasp app in minutes just from a title and short description!
:::

:::tip Try Wasp Without Installing 🤔?
  Give Wasp a spin in the browser with GitHub Codespaces by following the intructions in our [Tutorial App README](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoApp)
:::


### What next?

 - [ ] 👉 **Check out the [Todo App tutorial](../tutorial/01-create.md), which will take you through all the core features of Wasp!** 👈
 - [ ] [Setup your editor](./editor-setup.md) for working with Wasp.
 - [ ] Join us on [Discord](https://discord.gg/rzdnErX)! Any feedback or questions you have, we are there for you.
 - [ ] Follow Wasp development by subscribing to our newsletter: https://wasp-lang.dev/#signup . We usually send 1 per month, and [Matija](https://github.com/matijaSos) does his best to unleash his creativity to make them engaging and fun to read :D!

------

## More details

### Requirements

You must have Node.js (and NPM) installed on your machine and available in `PATH`.
A version of Node.js must be >= 18.

If you need it, we recommend using [nvm](https://github.com/nvm-sh/nvm) for managing your Node.js installation version(s).

<details>
  <summary style={{cursor: 'pointer', 'textDecoration': 'underline'}}>
    A quick guide on installing/using nvm
  </summary>
  <div>

  Install nvm via your OS package manager (`apt`, `pacman`, `homebrew`, ...) or via the [nvm](https://github.com/nvm-sh/nvm#install--update-script) install script.

  Then, install a version of Node.js that you need:
  ```shell
  nvm install 20
  ```

  Finally, whenever you need to ensure a specific version of Node.js is used, run:
  ```shell
  nvm use 20
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


### Installation

<Tabs
  defaultValue='linux/osx'
  values={[
    {label: 'Linux / macOS', value: 'linux/osx'},
    {label: 'Windows', value: 'win'},
    {label: 'From source', value: 'source'}
  ]}
>
  <TabItem value='linux/osx'>

Open your terminal and run:

```shell
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```

:::note Running Wasp on Mac with Mx chip (arm64)
**Experiencing the 'Bad CPU type in executable' issue on a device with arm64 (Apple Silicon)?**
Given that the wasp binary is built for x86 and not for arm64 (Apple Silicon), you'll need to install [Rosetta on your Mac](https://support.apple.com/en-us/HT211861) if you are using a Mac with Mx (M1, M2, ...). Rosetta is a translation process that enables users to run applications designed for x86 on arm64 (Apple Silicon). To install Rosetta, run the following command in your terminal
```bash
softwareupdate --install-rosetta
```
Once Rosetta is installed, you should be able to run Wasp without any issues.
:::

  </TabItem>

  <TabItem value='win'>

With Wasp for Windows, we are almost there: Wasp is successfully compiling and running on Windows but there is a bug or two stopping it from fully working. Check it out [here](https://github.com/wasp-lang/wasp/issues/48) if you are interested in helping.

In the meantime, the best way to start using Wasp on Windows is by using [WSL](https://learn.microsoft.com/en-us/windows/wsl/install). Once you set up Ubuntu on WSL, just follow Linux instructions for installing Wasp. You can refer to this [article](https://wasp-lang.dev/blog/2023/11/21/guide-windows-development-wasp-wsl) if you prefer a step by step guide to using Wasp in WSL environment. If you need further help, reach out to us on [Discord](https://discord.gg/rzdnErX) - we have some community members using WSL that might be able to help you.
:::caution
  If you are using WSL2, make sure that your Wasp project is not on the Windows file system, but instead on the Linux file system. Otherwise, Wasp won't be able to detect file changes, due to the [issue in WSL2](https://github.com/microsoft/WSL/issues/4739).
:::

  </TabItem>

  <TabItem value='source'>

If the installer is not working for you or your OS is not supported, you can try building Wasp from the source.

To install from source, you need to clone the [wasp repo](https://github.com/wasp-lang/wasp), install [Cabal](https://cabal.readthedocs.io/en/stable/getting-started.html) on your machine and then run `cabal install` from the `waspc/` dir.

If you have never built Wasp before, this might take some time due to `cabal` downloading dependencies for the first time.

Check [waspc/](https://github.com/wasp-lang/wasp/tree/main/waspc) for more details on building Wasp from the source.

  </TabItem>
</Tabs>
