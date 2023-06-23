---
id: 07-dependencies
title: "Dependencies"
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import { ShowForTs } from '@site/src/components/TsJsHelpers';

What is a Todo app without some clocks!? Well, still a Todo app, but certainly not as fun as one with clocks!

So, let's add a couple of clocks to our app, to help us track time while we perform our tasks (and to demonstrate the `app.dependencies` feature).

For this, we will use the `react-clock` library from NPM. We can add it to our project as a [dependency](language/features.md#dependencies) like this:
```wasp {4-6} title="main.wasp"
app TodoApp {
  // ...

  dependencies: [
    ("react-clock", "3.0.0")
  ]
}
```

Run
```shell
wasp start
```
to have Wasp download and install the new dependency. If `wasp start` is already running, Wasp will detect the dependency change, and restart automatically.

Next, let's create a new component `Clocks` where we can play with the clocks.
<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/Clocks.jsx"
import { useEffect, useState } from 'react'
import Clock from 'react-clock'
import 'react-clock/dist/Clock.css'

const Clocks = () => {
  const [time, setTime] = useState(new Date())

  useEffect(() => {
    const interval = setInterval(() => setTime(new Date()), 1000)
    return () => clearInterval(interval)
  }, [])

  return (
    <div style={{ display: 'flex' }}>
      <Clock value={time} />
      <Clock value={new Date(time.getTime() + 60 * 60000)} />
    </div>
  )
}

export default Clocks
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/Clocks.tsx"
import { useEffect, useState } from 'react'
import Clock from 'react-clock'
import 'react-clock/dist/Clock.css'

const Clocks = () => {
  const [time, setTime] = useState(new Date())

  useEffect(() => {
    const interval = setInterval(() => setTime(new Date()), 1000)
    return () => clearInterval(interval)
  }, [])

  return (
    <div style={{ display: 'flex' }}>
      <Clock value={time} />
      <Clock value={new Date(time.getTime() + 60 * 60000)} />
    </div>
  )
}

export default Clocks
```
</TabItem>
</Tabs>

And let's import it into our `MainPage.{jsx,tsx}` component.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx {2,11} title="src/client/MainPage.jsx"
// ...
import Clocks from './Clocks'

const MainPage = () => {
  // ...

  return (
    <div>
      // ...

      <div><Clocks /></div>

      // ...
    </div>
  )
}
// ...
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx {2,11} title="src/client/MainPage.tsx"
// ...
import Clocks from './Clocks'

const MainPage = () => {
  // ...

  return (
    <div>
      // ...

      <div><Clocks /></div>

      // ...
    </div>
  )
}
// ...
```
</TabItem>
</Tabs>

As you can see, importing other files from `src/client` is completely normal, just use the relative path. The same goes for all files under `src/server`. You can't (and shouldn't) import files from `src/client` into `src/server` and vice versa. If you want to share code between the two runtimes, you can use a relative import to import anything from `src/shared` into both the client code and the server code.

<ShowForTs>

The `src/shared` is especially handy for full-stack type definitions when using TypeScript.
</ShowForTs>

That is it! We added a dependency and used it in our project.
