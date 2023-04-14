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
- [express.json](https://expressjs.com/en/api.html#express.json) / [body-parser](https://github.com/expressjs/body-parser#bodyparserjsonoptions): Parses incoming request bodies in a middleware before your handlers, making the result available under the `req.body` property.
- [express.urlencoded](https://expressjs.com/en/api.html#express.urlencoded) \ [body-parser](https://expressjs.com/en/resources/middleware/body-parser.html#bodyparserurlencodedoptions): Returns middleware that only parses urlencoded bodies and only looks at requests where the `Content-Type` header matches the type option.
- [cookieParser](https://github.com/expressjs/cookie-parser#readme): Parse Cookie header and populate `req.cookies` with an object keyed by the cookie names.

## Customization

You have three places where you can customize middleware:
- global: here, any changes will apply by default *to all operations (`query` and `action`) and `api`.* Please use this with care.
- per-api: you can override middleware for a specific api route, for example `POST /foo/callback`. This is helpful if you want to disable JSON parsing, for example.
- per-path: this is helpful if you need to customize middleware for all methods for a given path. This is helpful for things like "complex CORS requests," which may need to apply to both `OPTIONS` and `GET`.

### Types

Below are the relevant TS types and the actual definitions of default middleware.

```ts
export type MiddlewareConfig = Map<string, express.RequestHandler>

export type MiddlewareConfigFn = (middleware: MiddlewareConfig) => MiddlewareConfig

const defaultGlobalMiddleware: MiddlewareConfig = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])
```

Here is the [definition of config](https://github.com/wasp-lang/wasp/blob/main/waspc/data/Generator/templates/server/src/config.js).

## Customize global middleware

## Customize `api`-specific middleware

## Customize path-path middleware
