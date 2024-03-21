---
title: Configuring Middleware
---
import { ShowForTs } from '@site/src/components/TsJsHelpers';

Wasp comes with a minimal set of useful Express middleware in every application. While this is good for most users, we realize some may wish to add, modify, or remove some of these choices both globally, or on a per-`api`/path basis.

## Default Global Middleware 🌍

Wasp's Express server has the following middleware by default:

- [Helmet](https://helmetjs.github.io/): Helmet helps you secure your Express apps by setting various HTTP headers. _It's not a silver bullet, but it's a good start._
- [CORS](https://github.com/expressjs/cors#readme): CORS is a package for providing a middleware that can be used to enable [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) with various options.

  :::note
  CORS middleware is required for the frontend to communicate with the backend.
  :::
- [Morgan](https://github.com/expressjs/morgan#readme): HTTP request logger middleware.
- [express.json](https://expressjs.com/en/api.html#express.json) (which uses [body-parser](https://github.com/expressjs/body-parser#bodyparserjsonoptions)): parses incoming request bodies in a middleware before your handlers, making the result available under the `req.body` property.

  :::note
  JSON middleware is required for [Operations](../data-model/operations/overview) to function properly.
  :::
- [express.urlencoded](https://expressjs.com/en/api.html#express.urlencoded) (which uses [body-parser](https://expressjs.com/en/resources/middleware/body-parser.html#bodyparserurlencodedoptions)): returns middleware that only parses urlencoded bodies and only looks at requests where the `Content-Type` header matches the type option.
- [cookieParser](https://github.com/expressjs/cookie-parser#readme): parses Cookie header and populates `req.cookies` with an object keyed by the cookie names.

## Customization

You have three places where you can customize middleware:
1. [global](#1-customize-global-middleware): here, any changes will apply by default *to all operations (`query` and `action`) and `api`.* This is helpful if you wanted to add support for multiple domains to CORS, for example.

  :::caution Modifying global middleware
  Please treat modifications to global middleware with extreme care as they will affect all operations and APIs. If you are unsure, use one of the other two options.
  :::

2. [per-api](#2-customize-api-specific-middleware): you can override middleware for a specific api route (e.g. `POST /webhook/callback`). This is helpful if you want to disable JSON parsing for some callback, for example.
3. [per-path](#3-customize-per-path-middleware): this is helpful if you need to customize middleware for all methods under a given path.
    - It's helpful for things like "complex CORS requests" which may need to apply to both `OPTIONS` and `GET`, or to apply some middleware to a _set of `api` routes_.

### Default Middleware Definitions

Below is the actual definitions of default middleware which you can override.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js
const defaultGlobalMiddleware = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts
export type MiddlewareConfig = Map<string, express.RequestHandler>

// Used in the examples below 👇
export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig

const defaultGlobalMiddleware: MiddlewareConfig = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])
```
</TabItem>
</Tabs>

## 1. Customize Global Middleware

If you would like to modify the middleware for _all_ operations and APIs, you can do something like:


<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {6} title=main.wasp
app todoApp {
  // ...

  server: {
    setupFn: import setup from "@src/serverSetup",
    middlewareConfigFn: import { serverMiddlewareFn } from "@src/serverSetup"
  },
}
```

```ts title=src/serverSetup.js
import cors from 'cors'
import { config } from 'wasp/server'

export const serverMiddlewareFn = (middlewareConfig) => {
  // Example of adding extra domains to CORS.
  middlewareConfig.set('cors', cors({ origin: [config.frontendUrl, 'https://example1.com', 'https://example2.com'] }))
  return middlewareConfig
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">


```wasp {6} title=main.wasp
app todoApp {
  // ...

  server: {
    setupFn: import setup from "@src/serverSetup",
    middlewareConfigFn: import { serverMiddlewareFn } from "@src/serverSetup"
  },
}
```

```ts title=src/serverSetup.ts
import cors from 'cors'
import { config, type MiddlewareConfigFn } from 'wasp/server'

export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // Example of adding an extra domains to CORS.
  middlewareConfig.set('cors', cors({ origin: [config.frontendUrl, 'https://example1.com', 'https://example2.com'] }))
  return middlewareConfig
}
```
</TabItem>
</Tabs>


## 2. Customize `api`-specific Middleware

If you would like to modify the middleware for a single API, you can do something like:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {5} title=main.wasp
// ...

api webhookCallback {
  fn: import { webhookCallback } from "@src/apis",
  middlewareConfigFn: import { webhookCallbackMiddlewareFn } from "@src/apis",
  httpRoute: (POST, "/webhook/callback"),
  auth: false
}
```

```ts title=src/apis.js
import express from 'express'

export const webhookCallback = (req, res, _context) => {
  res.json({ msg: req.body.length })
}

export const webhookCallbackMiddlewareFn = (middlewareConfig) => {
  console.log('webhookCallbackMiddlewareFn: Swap express.json for express.raw')

  middlewareConfig.delete('express.json')
  middlewareConfig.set('express.raw', express.raw({ type: '*/*' }))

  return middlewareConfig
}

```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp {5} title=main.wasp
// ...

api webhookCallback {
  fn: import { webhookCallback } from "@src/apis",
  middlewareConfigFn: import { webhookCallbackMiddlewareFn } from "@src/apis",
  httpRoute: (POST, "/webhook/callback"),
  auth: false
}
```

```ts title=src/apis.ts
import express from 'express'
import { type WebhookCallback } from 'wasp/server/api'
import { type MiddlewareConfigFn } from 'wasp/server'

export const webhookCallback: WebhookCallback = (req, res, _context) => {
  res.json({ msg: req.body.length })
}

export const webhookCallbackMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  console.log('webhookCallbackMiddlewareFn: Swap express.json for express.raw')

  middlewareConfig.delete('express.json')
  middlewareConfig.set('express.raw', express.raw({ type: '*/*' }))

  return middlewareConfig
}

```
</TabItem>
</Tabs>

:::note
This gets installed on a per-method basis. Behind the scenes, this results in code like:

```js
router.post('/webhook/callback', webhookCallbackMiddleware, ...)
```
:::

## 3. Customize Per-Path Middleware

If you would like to modify the middleware for all API routes under some common path, you can define a `middlewareConfigFn` on an `apiNamespace`:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp {4} title=main.wasp
// ...

apiNamespace fooBar {
  middlewareConfigFn: import { fooBarNamespaceMiddlewareFn } from "@src/apis",
  path: "/foo/bar"
}
```

```ts title=src/apis.js
export const fooBarNamespaceMiddlewareFn = (middlewareConfig) => {
  const customMiddleware = (_req, _res, next) => {
    console.log('fooBarNamespaceMiddlewareFn: custom middleware')
    next()
  }

  middlewareConfig.set('custom.middleware', customMiddleware)

  return middlewareConfig
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp {4} title=main.wasp
// ...

apiNamespace fooBar {
  middlewareConfigFn: import { fooBarNamespaceMiddlewareFn } from "@src/apis",
  path: "/foo/bar"
}
```

```ts title=src/apis.ts
import express from 'express'
import { type MiddlewareConfigFn } from 'wasp/server'

export const fooBarNamespaceMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  const customMiddleware: express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarNamespaceMiddlewareFn: custom middleware')
    next()
  }

  middlewareConfig.set('custom.middleware', customMiddleware)

  return middlewareConfig
}
```
</TabItem>
</Tabs>

:::note
This gets installed at the router level for the path. Behind the scenes, this results in something like:

```js
router.use('/foo/bar', fooBarNamespaceMiddleware)
```
:::
