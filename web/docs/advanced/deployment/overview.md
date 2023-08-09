---
title: Overview
---

import { DeploymentOptionsGrid } from './DeploymentOptionsGrid.tsx';

Wasp apps are full-stack apps that consist of:
- a Node.js server
- a static client
- and a PostgreSQL database. 

These parts of a Wasp app can be deployed **anywhere** where you can deploy Node.js apps or static apps. For example, you can deploy your client on [Netlify](https://www.netlify.com/), the server on [Fly.io](https://fly.io/) and the database on [Neon](https://neon.tech/).

To make things more convinent, we offer you one command deployment via the **Wasp CLI**, read more below.

<DeploymentOptionsGrid />

Regardless of how you choose to deploy your app (i.e., manually or using the Wasp CLI), you'll need to know about some common patterns covered below.

## Customizing the Dockerfile
By default, Wasp generates a multi-stage Dockerfile.
This file is used to build and run a Docker image with the Wasp-generated server code.
It also runs any pending migrations.

If you need to **customize this `Dockerfile`**, you may do so by adding a `Dockerfile` to your project root directory. If present, Wasp will append the contents of this file to the _bottom_ of our default Dockerfile.

Since the last definition in a Dockerfile wins, you can override or continue from any existing build stages. You could also choose not to use any of our build stages and have your own custom Dockerfile used as-is.

A few notes are in order:
- if you override an intermediate build stage, no later build stages will be used unless you reproduce them below
- the contents of the `Dockerfile` are dynamic, based on the features you use, and may change in future releases as well, so please verify the contents have not changed from time to time
- be sure to supply an `ENTRYPOINT` in your final build stage or it will not have any effect

Read more in the official Docker docs on [multi-stage builds](https://docs.docker.com/build/building/multi-stage/).

To see what your project's (potentially combined) `Dockerfile` will look like, run:
```shell
wasp dockerfile
```

Join our [Discord](https://discord.gg/rzdnErX) if you have any questions, or if you need more customization than this hook provides.
