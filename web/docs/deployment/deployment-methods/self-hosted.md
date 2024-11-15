---
title: Self-Hosted
---

If you have your own server or you rent out a server, you can self-host your Wasp apps. This deployment option has the benefit of giving you full control over your apps and its data. Also, it can be more cost-effective than using a cloud provider since you can deploy multiple apps on a single server.

## What you'll need

To successfully self-host your Wasp app, you need to have the following:
- A server with a public IP address
  - There are many cloud providers you can use to rent a server. Some popular ones are [AWS](https://aws.amazon.com/ec2/), [DigitalOcean](https://www.digitalocean.com/), [OVH](https://www.ovhcloud.com/en/vps/), and [Hetzner](https://www.hetzner.com/cloud/).
- A domain name (we'll need this to have HTTPS)

## Self-hosting steps

To self-host your Wasp app, you need to follow these general steps:
1. Get your app's code on the server. How you do this depends on your setup, we'll explore a few methods in the next section.
2. Set up [Docker](https://docs.docker.com/engine/install/) on the server. This is needed to run the server and the database.
3. Set up a reverse proxy to be able to use a domain name with your app.
4. Set up the server env variables.

![Self-hosting steps](/img/deploying/self-hosting.png)

## Deployment methods

We'll explore a few methods you can use to self-host your Wasp app. One is to use a simple setup where you manually set up the server. The other two methods will use self-hosted PaaS platforms that make it easy to deploy apps.

### Simple setup

In this setup, you install Docker on your server, set up the server env variable, set up a reverse proxy, and run your app. This is a very manual process, but you'll learn how everything works.

<details>
<summary>Overview of the steps</summary>

1. Install Docker, Node.js and Wasp on your server.
2. Set up a reverse proxy.
    - We are using [Caddy](https://caddyserver.com/) in our example, but you can use any reverse proxy you like (e.g., Nginx, Apache).
3. Get your app's code on the server with Git.
4. Build the app with Wasp CLI.
5. Build and run the server with Docker.
7. Start the database with Docker.
8. Set up Caddy to serve your client and server.
9. Point your domain to your server's IP address.

</details>

You can find our guide on how to deploy to a VPS with Docker and Caddy [here](https://gist.github.com/infomiho/80f3f50346566e39db56c5e57fefa1fe).

### Coolify

[Coolify](https://coolify.io/) is a self-hosted PaaS platform that makes it easy to deploy apps. It has a nice looking UI and it helps you with managing your deployments.

<details>
<summary>Overview of the steps</summary>

1. Install Coolify on your server.
2. Create your Coolify apps (client, server, and db).
3. Set up the server env variables.
4. Use Github Actions to build and upload your Docker images.
5. Trigger Coolify to pull the Docker images and deploy them.
6. Point your domain to your server's IP address.

</details>

You can find our guide on how to deploy your Wasp app with Coolify [here](https://gist.github.com/infomiho/ad6fade7396498ae32a931ca563a4524).

### CapRover

[CapRover](https://caprover.com/) is another self-hosted PaaS platform that makes it easy to deploy apps. It has been around longer than Coolify and it works great as well.

<details>
<summary>Overview of the steps</summary>

1. Install CapRover on your server.
2. Create your CapRover apps (client, server, and db).
3. Set up the server env variables.
4. Use Github Actions to build and upload your Docker images.
5. Deploy the new Docker images with CapRover.
6. Point your domain to your server's IP address.

</details>

You can find our guide on how to deploy your Wasp app with CapRover [here](https://gist.github.com/infomiho/a853e2f92aff6d52e9120b8974887464)