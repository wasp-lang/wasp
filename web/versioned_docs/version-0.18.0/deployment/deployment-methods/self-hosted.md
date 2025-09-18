---
title: Self-Hosted
---

import ImgWithCaption from '@site/blog/components/ImgWithCaption'
import { GuideLink } from './GuideLink.tsx'

If you have your server or rent out a server, you can self-host your Wasp apps. Self-hosting your apps gives you full control over your apps and their data. It can be more cost-effective than a cloud provider since you can deploy multiple apps on a single server. However, you'll need to manage the server yourself, which can be time-consuming and require some technical knowledge.

## What you'll need

To successfully self-host your Wasp app, you need to have the following:

- A server with a public IP address.
  - There are many cloud providers you can use to rent a server. Some popular ones are [AWS](https://aws.amazon.com/ec2/), [DigitalOcean](https://www.digitalocean.com/), [OVH](https://www.ovhcloud.com/en/vps/), and [Hetzner](https://www.hetzner.com/cloud/).
- A domain name, for example, `myapp.com` (needed for HTTPS support).

## Self-hosting steps

To self-host your Wasp app, you need to follow these general steps:

1. Get your **app's code** on the server.

   - How you do this depends on your setup, we'll explore a few methods in the next section.

2. Run your **client and server apps** on the server. Run the **database** on the server or use a managed database service.

   - We'll use [Docker](https://docs.docker.com/engine/install/) to run the server app and the database, but you can run them without Docker if you prefer.

3. Set up a **reverse proxy** on the server to be able to use a domain name with HTTPS for your app.

4. Configure the **env variables** on your server for the server app.

<ImgWithCaption source="/img/deploying/self-hosting.png" alt="One of many possible self-hosting setups" caption="One possible self-hosting setup" />

## Deployment methods

We'll explore a few methods you can use to self-host your Wasp app. The first method is the most straightforward: you manually set up everything on your server. The other two methods require you to install and configure a self-hosted PaaS on your server and then use that to deploy apps to it.

### Simple setup

In this setup, you do all the steps on the server: you install Docker, set up the server env variable, set up a reverse proxy, and run your app. This is a very manual process, but you'll learn how everything works.

#### Overview of the steps

On your server:

1. Install [Docker](https://docs.docker.com/engine/install/), [Node.js](https://github.com/nvm-sh/nvm) and [Wasp CLI](/introduction/quick-start.md#installation).
2. Get your **app's source code**.
   - We recommend using Git to clone your app's repository and then pulling the latest changes when you want to deploy a new version. You can use any other method to get your app's code on the server.
3. Build your app with **`wasp build`**.
4. Build and run the **server app**.
   - Wasp gives you a `Dockerfile` in the `.wasp/build` directory that you can use to build and run the server app.
   - We are using Docker to run the server app, but you can run it without Docker if you prefer - just make sure to replicate the setup in the `Dockerfile`.
   - When you run the server app with Docker, you need to setup the server env variables. You can do this with a `.env` file or by passing the env variables directly to the `docker run` command.
5. Start the **database** on the server or use a managed database service.
   - We usually run the database in Docker on the same server, but you can run the database directly on the server.
   - You can also use a managed database service which you can connect to from your server. This is a great option if you don't want to manage the database yourself, but it can be more expensive.
6. Build the **client app** into static files.
   - Wasp outputs the client app in the `.wasp/build/web-app` directory.
   <!-- TODO: we should change this link to the new place where we talk about how the client is built -->
   - You should [build the client app](./paas.md#3-deploying-the-web-client-) into static files.
7. Install and set up a **reverse proxy** to serve your client and server apps.
   - There are many great choices for reverse proxies, like [Nginx](https://www.nginx.com/), [Caddy](https://caddyserver.com/), and [Traefik](https://traefik.io/).
   - Make sure to set up the reverse proxy to serve the client app's static files and to proxy requests to the server app.
8. Point your **domain(s)** to your server's IP address.
   - We recommend setting `myapp.com` for the client and `api.myapp.com` for the server.
   - The reverse proxy should serve the client app on `myapp.com` and proxy requests to the server app on `api.myapp.com`. Make sure your [env variables](../env-vars.md) are using these client and server URLs.

Check out one of our step-by-step guides for more details:

<GuideLink linkToGuide="https://gist.github.com/infomiho/80f3f50346566e39db56c5e57fefa1fe" title="Deploying Wasp with Docker on your server" description="Uses Ubuntu, Git, Caddy, Docker" />

### Coolify

[Coolify](https://coolify.io/) is a deployment tool (self-hosted PaaS) that you run on your server. It makes it easier to deploy multple apps on your server. It has a nice looking UI and it helps you with managing your deployments.

#### Overview of the steps

1. Install [Coolify](https://coolify.io/) on your server.
2. Create your **Coolify apps** (client, server, and database).
   - You can run the database with Coolify on the same server, but you can run the database directly on your server or use a managed database service.
3. In Coolify, set up the **server app env variables**.
   - You can set up the env variables in the Coolify UI, check out which [env variables are required](../env-vars.md).
4. Set up some sort of **CI/CD** (for example [Github Actions](https://github.com/features/actions)) to:
   - build and upload your Docker images,
   - trigger Coolify to pull the Docker images and deploy them.
5. Point your domain to your server's IP address.
   - We recommend setting `myapp.com` for the client and `api.myapp.com` for the server.
   - Make sure to set the domains in the Coolify UI for the client and the server apps.
   - Make sure to set the env variables for the client and the server URLs correctly.

Check out one of our step-by-step guides for more details:

<GuideLink linkToGuide="https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524" title="Deploying Wasp with Coolify on your server" description="Uses Coolify, Github Actions, Github Container Registry" />

### CapRover

[CapRover](https://caprover.com/) is a deployment tool (self-hosted PaaS) that you run on your server. It makes it easier to deploy multple apps on your server. It has a nice looking UI and it helps you with managing your deployments.

#### Overview of the steps

1. Install [CapRover](https://caprover.com/) on your server.
2. Create your CapRover apps (client, server, and database).
   - You can run the database with CapRover on the same server, but you can run the database directly on your server or use a managed database service.
3. Set up the server env variables.
   - You can set up the env variables in the CapRover UI, check out which [env variables are required](../env-vars.md).
4. Set up some sort of **CI/CD** (for example [Github Actions](https://github.com/features/actions)) to:
   - build and upload your Docker images to a Docker registry,
   - trigger CapRover to pull the Docker images and deploy them.
5. Point your domain to your server's IP address.
   - We recommend setting `myapp.com` for the client and `api.myapp.com` for the server.
   - Make sure to set the domains in the CapRover UI for the client and the server apps.
   - Make sure to set the env variables for the client and the server URLs correctly.

Check out one of our step-by-step guides for more details:

<GuideLink linkToGuide="https://gist.github.com/infomiho/a853e2f92aff6d52e9120b8974887464" title="Deploying Wasp with Caprover on your server" description="Uses Caprover, Github Actions, Github Container Registry" />

## Database setup

<!-- TOPIC: database -->

In all of the guides, we run the **database on your server**. When you run the database on your server, you need to take care of backups, updates, and scaling. We suggest setting up [PostgresSQL periodic backups](https://tembo.io/docs/getting-started/postgres_guides/how-to-backup-and-restore-a-postgres-database) and/or taking snapshots of your server's disk. In case something bad happens to your server, you can restore your database from the backups.

If you prefer not to manage the database yourself, you can use a **managed database service**. The service provider takes care of backups, updates, and scaling for you but it can be more expensive than running the database on your server. Some popular managed database services are [AWS RDS](https://aws.amazon.com/rds/), [DigitalOcean Managed Databases](https://www.digitalocean.com/products/managed-databases/), and [Supabase](https://supabase.io/).
