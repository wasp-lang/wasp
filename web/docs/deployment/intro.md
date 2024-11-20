---
title: Introduction
---

After developing your app locally on your machine, the next step is to deploy it to the web so that others can access it.

In this section, we'll walk you through the steps to deploy your Wasp app.

### Wasp app structure

Before we start, let's understand the structure of a Wasp app.

A Wasp app consists of three main parts:
- **Client app**
  - It's a single-page application (SPA), built using [React](https://react.dev/). It's what the user sees and interacts with.
  - It's usually served by some static file server or you can host it on a CDN like Cloudflare or Netlify.

- **Server app**:
  - The backend of your app, built using [Express](https://expressjs.com/) on Node.js.
  - It handles requests from the client app, interacts with the database, and returns responses.
  - It's packaged using Docker images, so it can deployed anywhere that supports Docker.

- **Database**:
  - Wasp uses [PostgreSQL](https://www.postgresql.org/) as its production database.
  - You can host the database on your own server or use a cloud service.


![Wasp app structure](/img/deploying/wasp-app-flow.gif)

The thing to take away from this: the client app and server app are separate applications that communicate with each other over HTTP. They can be deployed independently of each other, we'll show you how to do that in the [deployment methods](./deployment-methods/overview.md) section.

Server needs to be able to communicate with the database, we'll show you how to set that up using [env variables](./env-vars.md).

### Deploying your app

In the following sections, we'll go through all the different things you need to know about deployment:

- How [env variables](./env-vars.md) work in production - they are different than using .env files in development
- Production [database setup](./database.md) - how migrations work, how to connect to the database, etc.
- Different deployment methods (using [Wasp's CLI](./deployment-methods/cli.md), [cloud services](./deployment-methods/paas.md), [self-hosting](./deployment-methods/self-hosted.md), etc.)
- How to [set up CI/CD](./ci-cd.md) for your app - automatically deploy your app when you push to your Git repository
- Some [extras](./extras.md) like custom domains, CDN, etc.



