# Interface: WebSocket

WebSocket configuration.

See [Web Sockets](https://wasp.sh/docs/advanced/web-sockets) for handler
shape and client-side usage.

## Example

```ts
import { app } from "@wasp.sh/spec"
import { webSocketFn } from "./src/webSocket" with { type: "ref" }

export default app({
  // ...
  webSocket: {
    fn: webSocketFn,
    autoConnect: true,
  },
})
```

## Properties

### autoConnect?

> `optional` **autoConnect?**: `boolean`

If `true` (the default), the client connects to the WebSocket server as
soon as the app loads. Set to `false` to connect manually via
`useSocket`.

#### Default

```ts
true
```

***

### fn

> **fn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Function that registers Socket.IO event handlers. Wasp calls it once on
server start with the Socket.IO server instance and a context containing
all app entities. If a connected socket is authenticated, Wasp stores the
user on `socket.data.user`.

See [the `websocketFn` docs](https://wasp.sh/docs/advanced/web-sockets#websocketfn).
