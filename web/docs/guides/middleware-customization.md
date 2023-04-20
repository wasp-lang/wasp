---
title: Middleware Customization
---
import useBaseUrl from '@docusaurus/useBaseUrl';

# Customizing Express server middleware

Wasp comes with a minimal set of useful Express middleware in every application. While this is good for most users, we realize some may wish to add, modify, or remove some of these choices both globally, or on a per-`api`/path basis.

## Default global middleware

- [Helmet](https://helmetjs.github.io/): Helmet helps you secure your Express apps by setting various HTTP headers. It's not a silver bullet, but it can help!
- [CORS](https://github.com/expressjs/cors#readme): CORS is a package for providing a middleware that can be used to enable [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) with various options.
- [Morgan](https://github.com/expressjs/morgan#readme): HTTP request logger middleware.
- [express.json](https://expressjs.com/en/api.html#express.json) / [body-parser](https://github.com/expressjs/body-parser#bodyparserjsonoptions): Parses incoming request bodies in a middleware before your handlers, making the result available under the `req.body` property. ⚠️ This is required for Wasp Operations to function properly.
- [express.urlencoded](https://expressjs.com/en/api.html#express.urlencoded) / [body-parser](https://expressjs.com/en/resources/middleware/body-parser.html#bodyparserurlencodedoptions): Returns middleware that only parses urlencoded bodies and only looks at requests where the `Content-Type` header matches the type option.
- [cookieParser](https://github.com/expressjs/cookie-parser#readme): Parse Cookie header and populate `req.cookies` with an object keyed by the cookie names.

## Customization

You have three places where you can customize middleware:
1. global: here, any changes will apply by default *to all operations (`query` and `action`) and `api`.* This is helpful if you wanted to add support for multiple domains to CORS, for example. ⚠️ Please treat modifications to global middleware with extreme care!
2. per-api: you can override middleware for a specific api route (exe: `POST /webhook/callback`). This is helpful if you want to disable JSON parsing, for example.
3. per-path: this is helpful if you need to customize middleware for all methods for a given path. This is helpful for things like "complex CORS requests" which may need to apply to both `OPTIONS` and `GET`, or to apply some middleware to a _set of `api` routes_.

### Types

Below are the relevant TS types and the actual definitions of default middleware.

```ts
export type MiddlewareConfig = Map<string, express.RequestHandler>

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

## 1. Customize global middleware

If you would like to modify the middleware for _all_ operations and APIs, you can do something like:

```c title=todoApp.wasp
app todoApp {
  // ...

  server: {
    setupFn: import setup from "@server/serverSetup.js",
    middlewareConfigFn: import { serverMiddlewareFn } from "@server/serverSetup.js"
  },
}
```

```ts title=src/server/serverSetup.js
import cors from 'cors'
import { MiddlewareConfigFn } from '@wasp/middleware'
import config from '@wasp/config.js'

export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // Example of adding an extra domains to CORS.
  middlewareConfig.set('cors', cors({ origin: [config.frontendUrl, 'https://example1.com', 'https://example2.com'] }))
  return middlewareConfig
}
```

## 2. Customize `api`-specific middleware

If you would like to modify the middleware for a single API, you can do something like:

```c title=todoApp.wasp
api webhookCallback {
  fn: import { webhookCallback } from "@server/apis.js",
  middlewareConfigFn: import { webhookCallbackMiddlewareFn } from "@server/apis.js",
  httpRoute: (POST, "/webhook/callback"),
  auth: false
}
```

```ts title=src/server/apis.ts
import express from 'express'
import { WebhookCallback } from '@wasp/apis/types'
import { MiddlewareConfigFn } from '@wasp/middleware'

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

:::note
This gets installed on a per-method basis. Behind the scenes, this results in something like:

```js
router.post('/webhook/callback', webhookCallbackMiddleware, ...)
```
:::

## 3. Customize path-path middleware

If you would like to modify the middleware for all API routes under some common path, you can do something like:

```c title=todoApp.wasp
namespace fooBar {
  middlewareConfigFn: import { fooBarNamespaceMiddlewareFn } from "@server/apis.js",
  path: "/foo/bar"
}
```

```ts title=src/server/apis.ts
export const fooBarNamespaceMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  const customMiddleware : express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarNamespaceMiddlewareFn: custom middleware')
    next()
  }

  middlewareConfig.set('custom.middleware', customMiddleware)

  return middlewareConfig
}
```

:::note
This gets installed at the router level for the path. Behind the scenes, this results in something like:

```js
router.use('/foo/bar', fooBarNamespaceMiddleware)
```
:::
