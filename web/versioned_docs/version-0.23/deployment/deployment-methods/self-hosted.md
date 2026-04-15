---
title: Self-Hosted
---

import { ImgWithCaption } from '@site/blog/components/ImgWithCaption'
import { GuideLink } from '@site/src/components/GuideLink'

If you have your server or rent out a server, you can self-host your Wasp apps. Self-hosting your apps gives you full control over your apps and their data. It can be more cost-effective than a cloud provider since you can deploy multiple apps on a single server. However, you'll need to manage the server yourself, which can be time-consuming and require some technical knowledge.

## Guides

We have step-by-step guides for deploying your Wasp app on your server with different methods. Check out the guides below:

<GuideLink linkToGuide="../../guides/deployment/caprover" title="Deploying Wasp with Caprover on your server" description="Uses Caprover, Github Actions, Github Container Registry" />

<GuideLink linkToGuide="../../guides/deployment/coolify" title="Deploying Wasp with Coolify on your server" description="Uses Coolify, Github Actions, Github Container Registry" />

<GuideLink linkToGuide="../../guides/deployment/vps" title="Deploying Wasp with Docker on your server" description="Uses Ubuntu, Git, Caddy, Docker" />

## Manual deployment

We will show you a general overview of the architecture of a self-hosted Wasp app and the steps you need to take to deploy your app on your server. This is a more manual process than using the guides above, but it gives you more control over your deployment and you'll learn how everything works. If you are looking for a more guided deployment, check out the guides above.

### What you'll need

To successfully self-host your Wasp app, you need to have the following:

- A server with a public IP address. There are many cloud providers you can use to rent a server. Some popular ones are [AWS](https://aws.amazon.com/ec2/), [DigitalOcean](https://www.digitalocean.com/), [OVH](https://www.ovhcloud.com/en/vps/), and [Hetzner](https://www.hetzner.com/cloud/).

- A domain name, for example, `myapp.com` (needed for HTTPS support).

### Architecture

To self-host your Wasp app, you'll follow these general steps:

1. From your **app's code**, let Wasp build a **server app** and a **client app**.
1. Set up the **server environment variables** on the server.
1. Run a **database** on the server or use a managed database service.
1. Run the **server app** on the server, with or without Docker.
1. Serve the **client app** with a static file server.
1. Set up a **reverse proxy** on the server to be able to use a domain name with HTTPS for your app.

<ImgWithCaption source="/img/deploying/self-hosting.png" alt="One of many possible self-hosting setups" caption="One possible self-hosting setup" />

### Steps

1. Install [Docker](https://docs.docker.com/engine/install/), [Node.js](https://github.com/nvm-sh/nvm) and [Wasp CLI](/introduction/quick-start.md#installation).
2. Get your **app's source code**.
   - We recommend using Git to clone your app's repository and then pulling the latest changes when you want to deploy a new version. You can use any other method to get your app's code on the server.
3. Build your app with **`wasp build`**.
4. Build and run the **server app**.
   - Wasp gives you a `Dockerfile` in the `.wasp/out` directory that you can use to build and run the server app.
   - We are using Docker to run the server app, but you can run it without Docker if you prefer - just make sure to replicate the setup in the `Dockerfile`.
   - When you run the server app with Docker, you need to setup the server env variables. You can do this with a `.env` file or by passing the env variables directly to the `docker run` command.
5. Start the **database** on the server or use a managed database service.
   - We usually run the database in Docker on the same server, but you can run the database directly on the server.
   - You can also use a managed database service which you can connect to from your server. This is a great option if you don't want to manage the database yourself, but it can be more expensive.
6. Build the **client app** into static files.
   - Wasp outputs the client app in the `.wasp/out/web-app` directory.
   <!-- TODO: we should change this link to the new place where we talk about how the client is built -->
   - You should [build the client app](./paas.md#3-deploying-the-web-client) into static files.
7. Install and set up a **reverse proxy** to serve your client and server apps.
   - There are many great choices for reverse proxies, like [Nginx](https://www.nginx.com/), [Caddy](https://caddyserver.com/), and [Traefik](https://traefik.io/).
   - Make sure to set up the reverse proxy to serve the client app's static files and to proxy requests to the server app.
8. Point your **domain(s)** to your server's IP address.
   - We recommend setting `myapp.com` for the client and `api.myapp.com` for the server.
   - The reverse proxy should serve the client app on `myapp.com` and proxy requests to the server app on `api.myapp.com`. Make sure your [env variables](../env-vars.md) are using these client and server URLs.

## Database setup

By default, our self-hosted deployment methods run the **database on your server**. When you run the database on your server, you need to take care of backups, updates, and scaling. We suggest setting up [PostgresSQL periodic backups](https://tembo.io/docs/getting-started/postgres_guides/how-to-backup-and-restore-a-postgres-database) and/or taking snapshots of your server's disk. In case something bad happens to your server, you can restore your database from the backups.

If you prefer not to manage the database yourself, you can use a **managed database service**. The service provider takes care of backups, updates, and scaling for you but it can be more expensive than running the database on your server. Some popular managed database services are [AWS RDS](https://aws.amazon.com/rds/), [DigitalOcean Managed Databases](https://www.digitalocean.com/products/managed-databases/), and [Supabase](https://supabase.io/).
