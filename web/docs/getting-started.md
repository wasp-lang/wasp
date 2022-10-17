---
title: Getting Started
slug: /
next: /tutorials/todo-app
---

import Tabs from '@theme/Tabs'
import TabItem from '@theme/TabItem'
import useBaseUrl from '@docusaurus/useBaseUrl';


## 1. Requirements

You need to have `node` (and `npm`) installed on your machine and available in `PATH`.
- `node`: 16.x.x
- `npm`: 8.x.x

You can check `node` and `npm` versions by running:
```shell-session
node -v
npm -v
```

We recommend using [nvm](https://github.com/nvm-sh/nvm) for managing your Node.js installation version(s).

<details>
  <summary style={{cursor: 'pointer', 'textDecoration': 'underline'}}>
    Quick guide on installing/using nvm
  </summary>
  <div>

  Install nvm via your OS package manager (aptitude, pacman, homebrew, ...) or alternatively via [nvm install script](https://github.com/nvm-sh/nvm#install--update-script).

  Then, install a version of node that you need, e.g.:
  ```shell-session
  nvm install 16
  ```

  Finally, whenever you need to ensure specific version of node is used, run e.g.
  ```shell-session
  nvm use 16
  ```
  to set the node version for current shell session.

  You can run
  ```shell-session
  node -v
  ```
  to check the version of node currently being used in this shell session.

  Check NVM repo for more details: https://github.com/nvm-sh/nvm .

  </div>
</details>

:::info

Why does Wasp require this specific `node` range and doesn't support a newer version x.y.z?

At Wasp, we focus on supporting the latest LTS ("long-term-support") Node.js version, since it guarantees stability and active maintainance, which is why the official Node.js team recommends it for usage in production.
Therefore, a specific Wasp release will usually require the version of Node.js that was LTS at that point of time.
Check out https://nodejs.org/en/about/releases/ for more details about Node.js releases.

Sometimes we will make an exception to that and additionally limit the Node.js version or postpone switching to the latest LTS if there are certain issues with new Node.js version, in which case we will catch up once those are resolved on Node.js side or we find a workaround on Wasp side.

:::

## 2. Installation

<Tabs
  defaultValue='linux/osx'
  values={[
    {label: 'Linux / OS X', value: 'linux/osx'},
    {label: 'Windows', value: 'win'},
    {label: 'From source', value: 'source'}
  ]}
>
  <TabItem value='linux/osx' >
<div style={{borderLeft: 'solid 6px #bf9900', paddingLeft: '10px'}} >

Open your terminal and run:

```bash
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```

</div>
  </TabItem>

  <TabItem value='win'>
<div style={{borderLeft: 'solid 6px #bf9900', paddingLeft: '10px'}} >

With Wasp for Windows, we are almost there: Wasp is successfully compiling and running on Windows but there is a bug or two stopping it from fully working. Check it out [here](https://github.com/wasp-lang/wasp/issues/48) if you are interested in helping.

In the meantime, the best way to start using Wasp on Windows is by using [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10). Once you set up Ubuntu on WSL, just follow Linux instructions for installing Wasp. If you need further help, reach out to us on [Discord](https://discord.gg/rzdnErX) - we have some community members using WSL that might be able to help you.

</div>
  </TabItem>

  <TabItem value='source'>
<div style={{borderLeft: 'solid 6px #bf9900', paddingLeft: '10px'}} >

If the installer is not working for you or your OS is not supported, you can try building Wasp from source.

To install from source, you need to clone the [wasp repo](https://github.com/wasp-lang/wasp), install [cabal](https://cabal.readthedocs.io/en/stable/getting-started.html) on your machine and then run `cabal install` from the `waspc/` dir.

If you have never built Wasp before, this might take some time due to `cabal` downloading dependencies for the first time.  

Check [waspc/](https://github.com/wasp-lang/wasp/tree/main/waspc) for more details on building.

</div>
  </TabItem>
</Tabs>

## 3. Creating and running your first app
```shell-session
wasp new MyNewApp # Creates a new web app named MyNewApp.
cd MyNewApp
wasp start # Serves the web app.
```

That's it :tada:! You have successfully created and served a new web app at <http://localhost:3000> and Wasp is serving both frontend and backend for you.



:::info For Visual Studio Code

If you are using VSCode, install our [Wasp language extention](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp). 

The extension has the following features tailored to meet your development needs:

* Syntax highlighting for .wasp files
* Snippets for .wasp files
* Wasp language server
* live reporting of compilation errors
* autocompletion

:::


## 4. What next?

**Check out the 🤓 [Todo App tutorial](tutorials/todo-app.md) 🤓 , which will take you through all the core features of Wasp!**

Also, we would be excited to have you join our community on [Discord](https://discord.gg/rzdnErX)! Any feedback or questions you have, we are there for you. 

