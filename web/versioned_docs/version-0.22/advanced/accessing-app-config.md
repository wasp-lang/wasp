---
title: Accessing the configuration 
---

Whenever you start a Wasp app, you are starting two processes.

- **The client process** - A React app that implements your app's frontend.

  During development, this is a dev server with hot reloading. In production,
  it's a simple process that serves pre-built static files with environment variables
  embedded during the build (details depend on [how you deploy it](../deployment/intro.md)).

- **The server process** - An Express server that implements your app's backend.

  During development, this is an Express server controlled by a
  [`nodemon`](https://www.npmjs.com/package/nodemon) process that takes care of
  hot reloading and restarts. In production, it's a regular Express server run
  using Node.

Check [the introduction](/introduction/introduction.md) for a more in-depth explanation of Wasp's runtime architecture.

You can configure both processes through environment variables. See [the deployment instructions](../project/env-vars.md) for a full list of supported variables.

Wasp gives you runtime access to the processes' configurations through **configuration objects**.

## Server configuration object

The server configuration object contains these fields:

- `frontendUrl: String` - Set it with env var `WASP_WEB_CLIENT_URL`.

  The URL of your client (the app's frontend).<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, you should set it to your client's URL as the server sees it
  (i.e., with the DNS and proxies considered).

You can access it like this:

```js
import { config } from 'wasp/server'

console.log(config.frontendUrl)
```

## Client configuration object

The client configuration object contains these fields:

- `apiUrl: String` - Set it with env var `REACT_APP_API_URL`

  The URL of your server (the app's backend).<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, it should contain the value of your server's URL as the user's browser
  sees it (i.e., with the DNS and proxies considered).

You can access it like this:

```js
import { config } from 'wasp/client'

console.log(config.apiUrl)
```
