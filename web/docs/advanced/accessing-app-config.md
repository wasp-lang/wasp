---
title: Accessing the configuration 
---

Whenever you start a Wasp app, you are starting one process: a server that serves your app's pages (a React app) and your app's API alike.

During development, that process is a Vite dev server with hot reloading for your client code and your server code. In production, it's a Node.js server built out of the same Vite config (details depend on [how you deploy it](../deployment/intro.md)).

You configure it through environment variables. See [the deployment instructions](../project/env-vars.md) for a full list of supported variables.

Your app's code still runs in two places, though: some of it in your users' browsers, and some of it on the server. Wasp gives each of them runtime access to its own configuration through **configuration objects**.

## Server configuration object

The server configuration object contains these fields:

- `frontendUrl: String` - Set it with env var `WASP_WEB_CLIENT_URL`.

  The URL your app's pages are served from.<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, it defaults to `WASP_SERVER_URL`, since one server serves your
  pages and your API. Set it only if you serve your pages from somewhere else.

You can access it like this:

```js
import { config } from 'wasp/server'

console.log(config.frontendUrl)
```

## Client configuration object

The client configuration object contains these fields:

- `apiUrl: String` - Set it with env var `REACT_APP_API_URL`

  Where your pages look for your app's API.<br />
  It defaults to your app's own origin, which is where its API is, so it is an
  empty string unless you set it. Set it only if you serve your API from
  somewhere else, to that URL as the user's browser sees it (i.e., with the DNS
  and proxies considered).

You can access it like this:

```js
import { config } from 'wasp/client'

console.log(config.apiUrl)
```
