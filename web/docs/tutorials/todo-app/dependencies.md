---
title: "Dependencies"
---

import useBaseUrl from '@docusaurus/useBaseUrl';

What is a Todo app without some clocks!? Well, still a Todo app, but certainly not as fun as one with the clocks!

So, let's add a couple of clocks to our app, to help us track time while we perform our tasks (and to demonstrate `dependencies` feature).

For this, we will use `react-clock` library from NPM. We can add it to our project as a [dependency](language/basic-elements.md#dependencies) like this:
```c title="main.wasp"
// ...

dependencies {=json
  "react-clock": "3.0.0"
json=}
```

Run (if it is already running, stop it first and then run it again)
```shell-session
wasp start
```
to have Wasp download and install new dependency (that happens on start of `wasp start`).

Next, let's create a new component `Clocks` where we can play with the clocks.
```jsx title="ext/Clocks.js"
import React, { useEffect, useState } from 'react'
import Clock from 'react-clock'
import 'react-clock/dist/Clock.css'

export default () => {
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
```

And let's import it in our main React component.
```jsx {2,11} title="ext/MainPage.js"
// ...
import Clocks from './Clocks'

const MainPage = () => {
  // ...

  return (
    <div>
      // ...

      <div> <Clocks /> </div>

      // ...
    </div>
  )
}
// ...
```
As you can see, importing other files from `/ext` is completely normal, just use the relative path.

That is it! We added a dependency and used it in our project.
