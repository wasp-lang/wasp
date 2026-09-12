---
title: Overview
---

import { CardLink } from '@site/src/components/CardLink';

Wasp apps are full-stack apps that consist of a Node.js server, a static client and a PostgreSQL database. How many things you deploy depends on your [deployment mode](../intro.md#deployment-modes):

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

You deploy **one app** and **one database**. `wasp build` builds the client and the server, and the generated Dockerfile packages them into a single image, which the server runs and serves the client from.

</TabItem>
<TabItem value="split" label="Split deployment">

You deploy **a static client**, **a server** and **one database**. `wasp build` builds only the server; you build the client yourself and upload it to a static host or CDN.

</TabItem>
</Tabs>

To make deploying as smooth as possible, Wasp also offers a single-command deployment called **Wasp Deploy**.

<CardLink
  kind="docs"
  to="./wasp-deploy/overview"
  title="Wasp Deploy"
  description="One-command deployment & redeployment"
/>

But even when not using Wasp Deploy, you can deploy the app **anywhere** where you can usually deploy Node.js apps or Docker images, and the database anywhere you can run PostgreSQL. For example, you can deploy your app on [Fly.io](https://fly.io/) and the database on [Neon](https://neon.tech/).

You can read our guides on how to deploy your Wasp app to different platforms, both from cloud providers and on your own infrastructure:

<CardLink
  kind="docs"
  to="./cloud-providers"
  title="Cloud Providers"
  description="Deploy your Wasp app to various cloud platforms"
/>

<CardLink
  kind="docs"
  to="./self-hosted"
  title="Self-hosted"
  description="Use your own servers to host your app"
/>

## Which method works with which mode {#which-method-works-with-which-mode}

Every method below works in both deployment modes, except the static hosts, which host a client and therefore only make sense in split mode.

| Method                                                                       | Single deployment          | Split deployment                          |
| ---------------------------------------------------------------------------- | ----------------------------- | ------------------------------ |
| [Wasp Deploy on Fly](./wasp-deploy/fly.md)                                    | One app plus the database     | Client and server apps plus the database |
| [Wasp Deploy on Railway](./wasp-deploy/railway.md)                            | One service plus the database | Client and server services plus the database |
| [Fly.io](../../guides/deployment/cloud-providers/flyio.md), [Railway](../../guides/deployment/cloud-providers/railway.md), [Heroku](../../guides/deployment/cloud-providers/heroku.md), [Render](../../guides/deployment/cloud-providers/render.md) | Yes | Yes, plus a static host for the client |
| [Netlify](../../guides/deployment/cloud-providers/netlify.md), [Cloudflare](../../guides/deployment/cloud-providers/cloudflare.md) | Not applicable, they host static files only | Yes, for the client |
| [Coolify](../../guides/deployment/self-hosted/coolify.md), [CapRover](../../guides/deployment/self-hosted/caprover.md), [your own VPS](../../guides/deployment/self-hosted/vps.md) | Yes | Yes |

Regardless of how you choose to deploy your app (i.e., manually or using the Wasp CLI), you'll need to know about some common patterns covered below.

:::tip Deployed? Get some swag! 👕🐝

Do you have a Wasp app running in production? If yes, we'd love to send some swag your way! All you need to do is
fill [this form](https://e44cy1h4s0q.typeform.com/to/EPJCwsMi) out and we'll make it happen.

:::

## Customizing the Dockerfile

By default, Wasp generates a multi-stage Dockerfile.
This file is used to build and run a Docker image with the Wasp-generated server code and the built client files, which the server serves.
It also runs any pending migrations.

You can **add extra steps to this multi-stage `Dockerfile`** by creating your own `Dockerfile` in the project's root directory.
If Wasp finds a Dockerfile in the project's root, it appends its contents at the _bottom_ of the default multi-stage Dockerfile.

Since the last definition in a Dockerfile wins, you can override or continue from any existing build stages.
You can also choose not to use any of our build stages and have your own custom Dockerfile used as-is.

A few things to keep in mind:

- If you override an intermediate build stage, no later build stages will be used unless you reproduce them below.
- The generated Dockerfile's content is dynamic and depends on which features your app uses. The content can also change in future releases, so please verify it from time to time.
- Make sure to supply `ENTRYPOINT` in your final build stage. Your changes won't have any effect if you don't.
- If your final stage starts from scratch instead of continuing from ours, copy the client build too (`COPY web-app/build .wasp/out/web-app/build`), or the server won't have anything to serve.

Read more in the official Docker docs on [multi-stage builds](https://docs.docker.com/build/building/multi-stage/).

To see what your project's (potentially combined) Dockerfile will look like, run:

```shell
wasp dockerfile
```

Join our [Discord](https://discord.gg/rzdnErX) if you have any questions, or if you need more customization than this hook provides.
