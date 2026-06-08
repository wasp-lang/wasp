---
title: Overview
title-llm: Deployment Overview
---

import { CardLink } from '@site/src/components/CardLink';

Wasp apps are full-stack apps that consist of:

- A Node.js server.
- A static client.
- A PostgreSQL database.

To make deploying as smooth as possible, Wasp also offers a single-command deployment called **Wasp Deploy**.

<CardLink
  kind="docs"
  to="./wasp-deploy/overview"
  title="Wasp Deploy"
  description="One-command deployment & redeployment"
/>

But even when not using Wasp Deploy, you can deploy each part **anywhere** where you can usually deploy Node.js apps or static apps. For example, you can deploy your client on [Netlify](https://www.netlify.com/), the server on [Fly.io](https://fly.io/), and the database on [Neon](https://neon.tech/).

You can read our guides on how to deploy your Wasp app to different platforms, both from cloud providers and on your own infrastructure:

<CardLink
  kind="guide"
  to="/docs/guides/deployment/cloud-providers"
  title="Cloud Providers"
  description="Deploy your Wasp app to various cloud platforms"
/>

<CardLink
  kind="guide"
  to="/docs/guides/deployment/self-hosted"
  title="Self-hosted"
  description="Use your own servers to host your app"
/>

Regardless of how you choose to deploy your app (i.e., manually or using the Wasp CLI), you'll need to know about some common patterns covered below.

:::tip Deployed? Get some swag! 👕🐝

Do you have a Wasp app running in production? If yes, we'd love to send some swag your way! All you need to do is
fill [this form](https://e44cy1h4s0q.typeform.com/to/EPJCwsMi) out and we'll make it happen.

:::

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
