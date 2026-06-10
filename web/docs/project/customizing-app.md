---
title: Customizing the App
---

import { Required } from '@site/src/components/Tag';

Each Wasp project can have only one `app` spec. It is used to configure your app and its components.

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "todoApp",
  wasp: {
    version: "{latestWaspVersion}",
  },
  title: "ToDo App",
  head: [
    "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap' />",
  ],
  // ...
})
```

We'll go through some common customizations you might want to do to your app. For more details on each of the fields, check out the [API Reference](#api-reference).

### Changing the App Title

You may want to change the title of your app, which appears in the browser tab, next to the favicon. You can change it by changing the `title` field of your `app` spec:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "myApp",
  wasp: {
    version: "{latestWaspVersion}",
  },
  title: "BookFace",
  // ...
})
```

### Adding Additional Lines to the Head

If you are looking to add additional style sheets or scripts to your app, you can do so by adding them to the `head` field of your `app` spec.

An example of adding extra style sheets and scripts:

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "myApp",
  wasp: {
    version: "{latestWaspVersion}",
  },
  title: "My App",
  head: [ // optional
    "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap' />",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js'></script>",
    "<meta name='viewport' content='minimum-scale=1, initial-scale=1, width=device-width' />",
  ],
  // ...
})
```

## API Reference

```ts title="main.wasp.ts"
import { app } from "@wasp.sh/spec"

export default app({
  name: "todoApp",
  wasp: {
    version: "{latestWaspVersion}",
  },
  title: "ToDo App",
  head: [
    "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap' />",
  ],
  auth: {
    // ...
  },
  client: {
    // ...
  },
  server: {
    // ...
  },
  db: {
    // ...
  },
  emailSender: {
    // ...
  },
  webSocket: {
    // ...
  },
  // ...
})
```

The `app` spec has the following fields:

- `wasp: object` <Required />
  Wasp compiler configuration. It is an object with a single field:

  - `version: string` <Required />

    The version specifies which versions of Wasp are compatible with the app. It should contain a valid [SemVer range](https://github.com/npm/node-semver#ranges).

- `title: string` <Required />

  Title of your app. It will appear in the browser tab, next to the favicon.

- `head: [string]`

  List of additional lines (e.g. `<link>` or `<script>` tags) to be included in the `<head>` of your HTML document. These need to be valid React JSX, so every tag should either be self-closing (e.g. `<meta ... />`) or have a matching closing tag (e.g. `<script>...</script>`).

  :::caution Script defer attribute
  Due to a [bug in React](https://github.com/facebook/react/issues/36169), the `defer` attribute on `<script>` tags causes hydration warnings. The workaround is to use `async` instead of `defer` for any `<script>` tags.
  :::

The rest of the fields are covered in dedicated sections of the docs:

- `auth: object`

  Authentication configuration. Read more in the [authentication section](../auth/overview) of the docs.

- `client: object`

  Configuration for the client side of your app. Read more in the [client configuration section](../project/client-config) of the docs.

- `server: object`

  Configuration for the server side of your app. Read more in the [server configuration section](../project/server-config) of the docs.

- `db: object`

  Database configuration. Read more in the [database configuration section](../data-model/databases.md) of the docs.

- `emailSender: object`

  Email sender configuration. Read more in the [email sending section](../advanced/email) of the docs.

- `webSocket: object`

  WebSocket configuration. Read more in the [WebSocket section](../advanced/web-sockets) of the docs.
