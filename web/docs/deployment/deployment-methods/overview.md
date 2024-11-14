---
title: Overview
---

import { DeploymentOptionsGrid } from './DeploymentOptionsGrid.tsx';

Wasp apps are full-stack apps that consist of:
- A Node.js server.
- A static client.
- A PostgreSQL database. 

You can deploy each part **anywhere** where you can usually deploy Node.js apps or static apps. For example, you can deploy your client on [Netlify](https://www.netlify.com/), the server on [Fly.io](https://fly.io/), and the database on [Neon](https://neon.tech/).

To make deploying as smooth as possible, Wasp also offers a single-command deployment through the **Wasp CLI**.

<DeploymentOptionsGrid />

Regardless of how you choose to deploy your app (i.e., manually or using the Wasp CLI), you'll need to know about some common patterns covered below.

## Customizing the Dockerfile
By default, Wasp generates a multi-stage Dockerfile.
This file is used to build and run a Docker image with the Wasp-generated server code.
It also runs any pending migrations.

You can **add extra steps to this multi-stage `Dockerfile`** by creating your own `Dockerfile` in the project's root directory.
If Wasp finds a Dockerfile in the project's root, it appends its contents at the _bottom_ of the default multi-stage Dockerfile.

Since the last definition in a Dockerfile wins, you can override or continue from any existing build stages.
You can also choose not to use any of our build stages and have your own custom Dockerfile used as-is.

A few things to keep in mind:

- If you override an intermediate build stage, no later build stages will be used unless you reproduce them below.
- The generated Dockerfile's content is dynamic and depends on which features your app uses. The content can also change in future releases, so please verify it from time to time.
- Make sure to supply `ENTRYPOINT` in your final build stage. Your changes won't have any effect if you don't.

Read more in the official Docker docs on [multi-stage builds](https://docs.docker.com/build/building/multi-stage/).

To see what your project's (potentially combined) Dockerfile will look like, run:
```shell
wasp dockerfile
```

Join our [Discord](https://discord.gg/rzdnErX) if you have any questions, or if you need more customization than this hook provides.
