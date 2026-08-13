---
title: Introduction
---

After developing your app locally on your machine, the next step is to deploy it to the web so that others can access it.

In this section, we'll walk you through the steps to deploy your Wasp app.

### Wasp app structure

Before we start, let's understand what Wasp generates when it builds your app.

What we call a "Wasp app" consists of two different parts:

- **The app**:
  - One Node.js server, built with [Nitro](https://nitro.build/), that serves everything your users touch: your pages (a [React](https://react.dev/) single-page application, with prerendered routes served as static HTML), your API, and your WebSockets.
  - It listens on one port and lives on one origin, so there is nothing to wire up between the front and the back of your app.
  - It comes with a ready-to-use `Dockerfile`, so you can package it and deploy it anywhere Docker is supported.

- **Database**:
  - Wasp uses [PostgreSQL](https://www.postgresql.org/) as its production database.
  - You can host the database on your own server or use a cloud service.

The thing to take away from this: deploying a Wasp app means deploying one thing, plus a database for it to talk to.

We'll show you different ways of how deploy your app in the [deployment methods](./deployment-methods/overview.md) section.

Your app needs to be able to communicate with the database, we'll show you how to set that up using [env variables](./env-vars.md).

### Deploying your app

In the following sections, we'll go through all the different things you need to know about deployment:

- How [env variables](./env-vars.md) work in production - they are different than using .env files in development.
- Production [database setup](./database.md) - how migrations work, how to connect to the database, etc.
- Different deployment methods (using [Wasp's CLI](./deployment-methods/wasp-deploy/overview.md), [cloud services](./deployment-methods/cloud-providers.md), [self-hosting](./deployment-methods/self-hosted.md), etc.)
- How to [set up CI/CD](./ci-cd.md) for your app - automatically deploy your app when you push to your Git repository.
- Some [extras](./extras.md) like custom domains, CDN, etc.
