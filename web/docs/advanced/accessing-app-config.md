---
title: Accessing the configuration 
---

Whenever you start a Wasp app, you are starting two processes.

- **The client process** - A React app that implements your app's frontend.

  During development, this is a dev server with hot reloading, which proxies
  Wasp's routes to the server so the browser only ever talks to one origin. In
  production, the client is a set of pre-built static files with environment
  variables embedded during the build, served by the server process (or by a
  static host under [split hosting](../deployment/intro.md#deployment-modes)).

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

  Your app's public URL, for example `https://myapp.com`.<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, you should set it to your app's URL as the user's browser sees it
  (i.e., with the DNS and proxies considered).

- `frontendUrl: String` - Derived from env var `WASP_WEB_CLIENT_URL`, which defaults to `WASP_SERVER_URL`.

  The URL of your client (the app's frontend).<br />
  Wasp automatically sets it during development when you run `wasp start`.<br />
  In production, it is the origin of `WASP_WEB_CLIENT_URL` (or of `WASP_SERVER_URL` when
  that is not set) plus `client.baseDir`, so it equals your app's URL unless you host the
  client separately. Under [split hosting](../deployment/intro.md#deployment-modes)
  it is `WASP_WEB_CLIENT_URL` exactly as you set it.

- `clientBaseDir: String` - Comes from [`client.baseDir`](../project/client-config.md#base-directory) in your Wasp file.

  The path under which your pages live, `/` by default.

You can access them like this:

```js
import { config } from 'wasp/server'

console.log(config.serverUrl)
console.log(config.frontendUrl)
```

## Client configuration object

The client configuration object contains these fields:

- `apiUrl: String`

  The URL the client sends its API requests to. Where it comes from depends on your
  [deployment mode](../deployment/intro.md#deployment-modes):

<Tabs groupId="deployment-mode">
<TabItem value="single" label="Single deployment">

In the browser this is the page's own origin (`window.location.origin`): the server serves the
client in production, and during development the client dev server proxies Wasp's routes to the
server, so the client never needs the server's address.

Outside the browser (for example while [prerendering](./prerendering.md)) there is no page origin,
so it comes from the `REACT_APP_API_URL` env var, or is an empty string if that is not set.

</TabItem>
<TabItem value="split" label="Split deployment">

The client is hosted separately from the server, so it always comes from the `REACT_APP_API_URL`
env var you build the client with.

</TabItem>
</Tabs>

You can access it like this:

```js
import { config } from 'wasp/client'

console.log(config.apiUrl)
```
