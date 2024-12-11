---
title: Self-Hosted
---

If you have your server or you rent out a server, you can self-host your Wasp apps. Self-hosting your apps gives you full control over your apps and their data. It can be more cost-effective than using a cloud provider since you can deploy multiple apps on a single server. However, you'll need to manage the server yourself, which can be time-consuming and require some technical knowledge.

## What you'll need

To successfully self-host your Wasp app, you need to have the following:

- A server with a public IP address.
  - There are many cloud providers you can use to rent a server. Some popular ones are [AWS](https://aws.amazon.com/ec2/), [DigitalOcean](https://www.digitalocean.com/), [OVH](https://www.ovhcloud.com/en/vps/), and [Hetzner](https://www.hetzner.com/cloud/).
- A domain name, for example `myapp.com` (needed for HTTPS support).

## Self-hosting steps

To self-host your Wasp app, you need to follow these general steps:

1. Get your app's code on the server.

   - How you do this depends on your setup, we'll explore a few methods in the next section.

2. Set up [Docker](https://docs.docker.com/engine/install/) on the server.

   - We'll use Docker to run the app and the database, but you can run them without Docker if you prefer.

3. Set up a reverse proxy to be able to use a domain name with HTTPS for your app.
4. Set up the server env variables.

![Self-hosting steps](/img/deploying/self-hosting.png)

## Deployment methods

We'll explore a few methods you can use to self-host your Wasp app. The first method is the most straightforward: you manually set up everything on your server. The other two methods require you to install and configure a self-hosted PaaS on your server, and then use that to deploy apps to it.

### Simple setup

In this setup, you do all the steps on the server: you install Docker, set up the server env variable, set up a reverse proxy, and run your app. This is a very manual process, but you'll learn how everything works.

<details>
<summary>Overview of the steps</summary>

On your server:

1. Install Docker, Node.js and Wasp CLI.
2. Get your app's source code.
   - We are using git in our full guide, but you can use any method you like.
3. Build your app with the Wasp CLI.
4. Build and run the server app with Docker.
5. Start the database with Docker.
   - We are running the database in Docker on the same server, but you can run the database directly on the server or use a managed database service.
6. Install and set up a reverse proxy to serve your client and server apps.
   - We are using [Caddy](https://caddyserver.com/) in our full guide , but you can use any reverse proxy you like (e.g., Nginx, Apache).
7. Point your domains to your server's IP address.
   - We are using `myapp.com` for the client and `api.myapp.com` for the server in our full guide.

</details>

Read our [full guide](https://gist.github.com/infomiho/80f3f50346566e39db56c5e57fefa1fe) on deploying Wasp apps to your server.

### Coolify

[Coolify](https://coolify.io/) is a deployment tool (self-hosted PaaS) that you run on your server. It makes it easier to deploy multple apps on your server. It has a nice looking UI and it helps you with managing your deployments.

<details>
<summary>Overview of the steps</summary>

1. Install Coolify on your server.
2. Create your Coolify apps (client, server, and database).
   - We are running the database with Coolify on the same server, but you can run the database directly on the server or use a managed database service.
3. In Coolify, set up the server app env variables.
4. Set up Github Actions to:
   - build and upload your Docker images,
   - trigger Coolify to pull the Docker images and deploy them.
5. Point your domain to your server's IP address.
   - We are using `myapp.com` for the client and `api.myapp.com` for the server in our full guide.

</details>

Read our [full guide](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524) on deploying Wasp apps with Coolify.

### CapRover

[CapRover](https://caprover.com/) is a deployment tool (self-hosted PaaS) that you run on your server. It makes it easier to deploy multple apps on your server. It has a nice looking UI and it helps you with managing your deployments.

<details>
<summary>Overview of the steps</summary>

1. Install CapRover on your server.
2. Create your CapRover apps (client, server, and db).
3. Set up the server env variables.
4. Use Github Actions to build and upload your Docker images.
5. Deploy the new Docker images with CapRover.
6. Point your domain to your server's IP address.

</details>

Read our [full guide](https://gist.github.com/infomiho/a853e2f92aff6d52e9120b8974887464) on deploying Wasp apps with CapRover.

## Database setup

In all of the guides, we run the **database on your server**. When you run the database on your server, you need to take care of backups, updates, and scaling. We suggest setting up [PostgresSQL periodic backups](https://tembo.io/docs/getting-started/postgres_guides/how-to-backup-and-restore-a-postgres-database) and/or taking snapshots of your server's disk. In case something bad happens to your server, you can restore your database from the backups.

If you prefer not to manage the database yourself, you can use a **managed database service**. The service provider takes care of backups, updates, and scaling for you but it can be more expensive than running the database on your server. Some popular managed database services are [AWS RDS](https://aws.amazon.com/rds/), [DigitalOcean Managed Databases](https://www.digitalocean.com/products/managed-databases/), and [Supabase](https://supabase.io/).
