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

- `serverUrl: String` - Set it with env var `WASP_SERVER_URL`.

  The origin of your server, for example `https://api.myapp.com`, without the base path.<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, you should set it to your server's URL as the user's browser sees it
  (i.e., with the DNS and proxies considered).

- `serverBasePath: String` - Comes from [`server.basePath`](../project/server-config.md#base-path) in your Wasp file.

  The path under which the whole server lives, for example `/api`: Wasp's routes,
  your `api`s and the routes you add in `setupFn`. It is `""` when the base path is `/`.
  Server URLs are `serverUrl + serverBasePath + route`, so an `api` declared at
  `/foo/bar` is at `${config.serverUrl}${config.serverBasePath}/foo/bar`.

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

- `apiUrl: String` - Derived from env var `REACT_APP_API_URL`.

  The URL of the API: the server's origin plus its [base path](../project/server-config.md#base-path),
  for example `https://api.myapp.com/api`. Everything the server answers lives under it, so
  `${config.apiUrl}/foo/bar` is your `api` declared at `/foo/bar`.<br />
  Wasp sets it automatically during development when you run `wasp start`.<br />
  In production, it comes from `REACT_APP_API_URL`, which you set to your server's origin as the user's browser
  sees it (i.e., with the DNS and proxies considered), without the base path.

You can access it like this:

```js
import { config } from 'wasp/client'

console.log(config.apiUrl)
```
