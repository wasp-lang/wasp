---
title: Customizing the App
---

import { CardLink } from '@site/src/components/CardLink';

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
    "<link rel='icon' href='/favicon.ico' />",
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
    "<link rel='icon' href='/favicon.ico' />",
    "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap' />",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js'></script>",
    "<meta name='viewport' content='minimum-scale=1, initial-scale=1, width=device-width' />",
  ],
  // ...
})
```

## API Reference

<CardLink
  to="../api/@wasp.sh/spec/interfaces/App"
  kind="api"
  title="App"
  description="All the options for the app spec."
/>
