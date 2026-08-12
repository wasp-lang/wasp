---
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";

# Cloudflare

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.26", "Cloudflare Workers": new Date("2026-04-06") }} />

## Cloudflare Workers Can't Host a Wasp App

A Wasp app is not a static site. `wasp build` produces a single server that serves your app's pages, its static assets and its API from one origin, and it needs a PostgreSQL database to talk to. There is no separate client bundle to upload anywhere.

Cloudflare Workers serve static assets and run code on a JavaScript runtime that isn't Node.js. They have nowhere to run your app's server, so they can't host a Wasp app on their own.

## Where to Deploy Instead

Deploy your app to a provider that runs containers:

- [Fly.io](./flyio.md)
- [Railway](./railway.md)
- [Render](./render.md)
- [Heroku](./heroku.md)
- [Your own server](../../../deployment/deployment-methods/self-hosted.md)

See [Cloud Providers](../../../deployment/deployment-methods/cloud-providers.md) for all of our deployment guides.

You can still put Cloudflare in front of a deployed Wasp app for its CDN and DDoS protection. See [Extras](../../../deployment/extras.md) for more on that.
