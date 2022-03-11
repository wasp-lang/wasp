---
title: Getting Started
slug: /
next: /tutorials/todo-app
---

import Tabs from '@theme/Tabs'
import TabItem from '@theme/TabItem'
import useBaseUrl from '@docusaurus/useBaseUrl';


## 1. Requirements

### Node.js compatible with 16.0.0
```shell-session
node -v  # must be something like 16.x.x
```

### NPM compatible with 8.0.0
```shell-session
npm -v  # must be something like 8.x.x
```

We recommend using [nvm](https://github.com/nvm-sh/nvm) for managing your Node.js installation version(s).

<details>
  <summary style={{cursor: 'pointer', 'textDecoration': 'underline'}}>
    Quick guide on installing/using nvm
  </summary>
  <div>

  Install nvm via your OS package manager (aptitude, pacman, homebrew, ...) or alternatively via [nvm install script](https://github.com/nvm-sh/nvm#install--update-script).

  Then, install a version of node that you need (any version compatible with 16.0.0), e.g.:
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

If installer is not working for you or your OS is not supported, you can try building Wasp from source.

To install from source, you need to clone the [wasp repo](https://github.com/wasp-lang/wasp), install [stack](https://docs.haskellstack.org) on your machine and then run `stack install` from the `waspc/` dir.

If you have never built Wasp before, this might take some time due to `stack` downloading dependencies for the first time.

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

## 4. What next?

**Check out the 🤓 [Todo App tutorial](tutorials/todo-app.md) 🤓 , which will take you through all the core features of Wasp!**

You can also:
- Join the community on [Discord](https://discord.gg/rzdnErX)! Any feedback or questions you have, we are there for you.
- If you are using Visual Studio Code, install our <a href="https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp">Wasp language extension</a>!
