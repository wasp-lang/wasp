---
title: Examples
---

import useBaseUrl from '@docusaurus/useBaseUrl';

We have a constantly growing collection of fully-functioning example apps, which you can use to learn more about Wasp's features.

The full list of examples can be found [here](https://github.com/wasp-lang/wasp/tree/release/examples/). Here is a few of them:

## Todo App
 - **Features**: Auth ([username/password](language/features#authentication--authorization)), [Queries & Actions](language/features#queries-and-actions-aka-operations), [Entities](language/features#entity), [Routes](language/features#route)
 - JS source code: [GitHub](https://github.com/wasp-lang/wasp/tree/release/examples/tutorials/TodoApp)
 - TS source code: [GitHub](https://github.com/wasp-lang/wasp/tree/release/examples/todo-typescript)
 - in-browser dev environment: [GitPod](https://gitpod.io/#https://github.com/wasp-lang/gitpod-template)

## Waspello (Trello Clone)
 - **Features**: Auth ([Google](language/features#social-login-providers-oauth-20---google-github), [username/password](language/features#authentication--authorization)), [Optimistic Updates](language/features#the-useaction-hook), [Tailwind CSS integration](integrations/css-frameworks)
 - Source code: [GitHub](https://github.com/wasp-lang/wasp/tree/main/examples/waspello)
 - Hosted at [https://waspello-demo.netlify.app](https://waspello-demo.netlify.app/login)
 <p align='center'>
<img src={useBaseUrl('img/wespello-new.png')} width='75%'/>
</p>

## Waspleau (Realtime Statistics Dashboard)
  - **Features**: Cron [Jobs](language/features#jobs), [Server Setup](language/features#server-configuration)
  - Source code: [GitHub](https://github.com/wasp-lang/wasp/tree/main/examples/waspleau)
  - Hosted at [https://waspleau.netlify.app/](https://waspleau.netlify.app/)
   <p align='center'>
<img src={useBaseUrl('img/waspleau.png')} width='75%'/>
</p>