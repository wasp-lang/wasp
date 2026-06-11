# Function: app()

> **app**(`config`): [`App`](../interfaces/App.md)

Creates a Wasp [App](../interfaces/App.md).

Call `app()` exactly once in your `main.wasp.ts` and export the result as
the file's default export. The Wasp compiler reads this default export to
generate your app.

See the [Wasp Spec docs](https://wasp.sh/docs/general/spec) for the full
shape of the configuration.

## Parameters

### config

The app configuration.

#### auth?

[`Auth`](../interfaces/Auth.md)

Configuration for authentication. Enables auth when set.

#### client?

[`Client`](../interfaces/Client.md)

Configuration for the client part of the resulting Wasp app.

#### db?

[`Db`](../interfaces/Db.md)

Configuration for the app's database.

#### emailSender?

[`EmailSender`](../interfaces/EmailSender.md)

Configuration for the app's email sender.

#### head?

`string`[]

Extra tags injected into the HTML `<head>`.

Each entry is rendered inside a React component, so the strings must be
valid JSX: self-closing tags must end with `/>` (e.g. `<meta ... />`),
and attributes must be camelCased (e.g. `httpEquiv` instead of
`http-equiv`).

Due to a [React bug](https://github.com/facebook/react/issues/36169),
avoid `defer` on `<script>` tags because it can cause hydration
warnings. Use `async` instead.

#### name

`string`

Internal app name.

Must not contain spaces.

#### server?

[`Server`](../interfaces/Server.md)

Configuration for the server part of the resulting Wasp app.

#### spec

[`Spec`](../type-aliases/Spec.md)

The specification ([Spec](../type-aliases/Spec.md)) of the app.

Build entries with the dedicated constructors ([page](page.md), [route](route.md),
[query](query.md), [action](action.md), [api](api.md), [apiNamespace](apiNamespace.md),
[job](job.md), [crud](crud.md)).

#### title

`string`

App title.

Used as the browser tab title.

#### wasp

[`Wasp`](../interfaces/Wasp.md)

Wasp metadata.

This will get checked against the Wasp CLI.

#### webSocket?

[`WebSocket`](../interfaces/WebSocket.md)

Configuration for the app's WebSocket support.

## Returns

[`App`](../interfaces/App.md)

## Example

```ts
import { app, page, route } from '@wasp.sh/spec'
import MainPage from './src/MainPage' with { type: 'ref' }

export default app({
  name: 'todoApp',
  title: 'ToDo App',
  wasp: { version: '^0.24.0' },
  spec: [
    route('MainRoute', '/', page(MainPage)),
  ],
})
```
