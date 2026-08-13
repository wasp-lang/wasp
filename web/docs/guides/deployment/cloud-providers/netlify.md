---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Netlify

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", Netlify: new Date("2026-05-28") }} />

## Netlify Can't Host a Wasp App

A Wasp app is not a static site. `wasp build` produces a single server that serves your app's pages, its static assets and its API from one origin, and it needs a PostgreSQL database to talk to. There is no separate client bundle to upload anywhere.

Netlify hosts static files and serverless functions. It has nowhere to run your app's server, so it can't host a Wasp app on its own.

## Where to Deploy Instead

Deploy your app to a provider that runs containers:

- [Fly.io](./flyio.md)
- [Railway](./railway.md)
- [Render](./render.md)
- [Heroku](./heroku.md)
- [Your own server](../../../deployment/deployment-methods/self-hosted.md)

See [Cloud Providers](../../../deployment/deployment-methods/cloud-providers.md) for all of our deployment guides.
