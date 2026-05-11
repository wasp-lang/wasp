### Handling Polar Webhook Signatures (Raw Body)

Polar's `validateEvent` function relies on an HMAC-SHA256 signature. This cryptographic validation requires the exact raw payload (Buffer) sent by Polar's servers. 

By default, Wasp applies a global `express.json()` middleware that parses the incoming request into a JavaScript object. This alters the payload and causes signature validation to fail (resulting in a 403 Unauthorized error, especially during local testing with ngrok).

To fix this, you must override the middleware for the Polar webhook route in your `main.wasp` file:

```wasp
api polarWebhook {
  fn: import { polarWebhook } from "@src/webhooks/polar.js",
  httpRoute: (POST, "/webhook/polar"),
  middlewareConfigFn: import { polarWebhookMiddleware } from "@src/webhooks/middleware.js"
}

```
Next, create the middleware file to strip the JSON parser and inject the raw
parser:
```javascript

// src/webhooks/middleware.js
import express from 'express'

export const polarWebhookMiddleware = (middlewareConfig) => {
  // 1. Remove the global JSON parser that destroys the signature
  middlewareConfig.delete('express.json')
  
  // 2. Inject the raw parser required by Polar's SDK
  middlewareConfig.set('express.raw', express.raw({ type: 'application/json' }))
  
  return middlewareConfig
}
```

With this configuration, req.body will be passed as a raw Buffer to your
polarWebhook function, allowing validateEvent to compute the hash correctly.
