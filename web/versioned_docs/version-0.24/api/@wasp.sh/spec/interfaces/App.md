# Interface: App

Root shape of a Wasp app specification.

Pass an `App` to the [app](../functions/app.md) constructor and `export default` the
result from `main.wasp.ts`.

## Example

```ts
import { app } from "@wasp.sh/spec"

export default app({
  name: "todoApp",
  wasp: { version: "^0.24.0" },
  title: "ToDo App",
  head: ["<link rel='icon' href='/favicon.ico' />"],
  spec: [],
});
```

## Properties

### auth?

> `optional` **auth?**: [`Auth`](Auth.md)

Configuration for authentication. Enables auth when set.

***

### client?

> `optional` **client?**: [`Client`](Client.md)

Configuration for the client part of the resulting Wasp app.

***

### db?

> `optional` **db?**: [`Db`](Db.md)

Configuration for the app's database.

***

### emailSender?

> `optional` **emailSender?**: [`EmailSender`](EmailSender.md)

Configuration for the app's email sender.

***

### head?

> `optional` **head?**: `string`[]

Extra tags injected into the HTML `<head>`.

Each entry is rendered inside a React component, so the strings must be
valid JSX: self-closing tags must end with `/>` (e.g. `<meta ... />`),
and attributes must be camelCased (e.g. `httpEquiv` instead of
`http-equiv`).

Due to a [React bug](https://github.com/facebook/react/issues/36169),
avoid `defer` on `<script>` tags because it can cause hydration
warnings. Use `async` instead.

***

### name

> **name**: `string`

Internal app name.

Must not contain spaces.

***

### server?

> `optional` **server?**: [`Server`](Server.md)

Configuration for the server part of the resulting Wasp app.

***

### spec

> **spec**: [`Spec`](../type-aliases/Spec.md)

The specification ([Spec](../type-aliases/Spec.md)) of the app.

Build entries with the dedicated constructors ([page](../functions/page.md), [route](../functions/route.md),
[query](../functions/query.md), [action](../functions/action.md), [api](../functions/api.md), [apiNamespace](../functions/apiNamespace.md),
[job](../functions/job.md), [crud](../functions/crud.md)).

***

### title

> **title**: `string`

App title.

Used as the browser tab title.

***

### wasp

> **wasp**: [`Wasp`](Wasp.md)

Wasp metadata.

This will get checked against the Wasp CLI.

***

### webSocket?

> `optional` **webSocket?**: [`WebSocket`](WebSocket.md)

Configuration for the app's WebSocket support.
