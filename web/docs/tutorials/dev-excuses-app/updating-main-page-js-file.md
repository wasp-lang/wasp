---
title: Updating MainPage.js file
---

import useBaseUrl from '@docusaurus/useBaseUrl';

This one is the most complex one, but it really comes down to mostly writing React. To make our life easier - let’s erase everything we had in the `MainPage.js` file and substitute it with our new UI markup.

```js title=".../ext/MainPage.js | Updating the UI"
import React, { useState } from 'react'
import { useQuery } from '@wasp/queries'
import getExcuse from '@wasp/queries/getExcuse'
import getAllSavedExcuses from '@wasp/queries/getAllSavedExcuses'
import saveExcuse from '@wasp/actions/saveExcuse'

const MainPage = () => {
  const [currentExcuse, setCurrentExcuse] = useState({ text: "" })
  const { data: excuses } = useQuery(getAllSavedExcuses)

  const handleGetExcuse = async () => {
    try {
      setCurrentExcuse(await getExcuse())
    } catch (err) {
      window.alert('Error while getting the excuse: ' + err.message)
    }
  }

  const handleSaveExcuse = async () => {
    if (currentExcuse.text) {
      try {
        await saveExcuse(currentExcuse)
      } catch (err) {
        window.alert('Error while saving the excuse: ' + err.message)
      }
    }
  }

  return (
    <div className="grid grid-cols-2 text-3xl">
      <div>
          <button onClick={handleGetExcuse} className="mx-2 my-1 p-2 bg-blue-600 hover:bg-blue-400 text-white rounded"> Get excuse </button>
          <button onClick={handleSaveExcuse} className="mx-2 my-1 p-2 bg-blue-600 hover:bg-blue-400 text-white rounded"> Save excuse </button>
        <Excuse excuse={currentExcuse} />
      </div>
      <div>
        <div className="px-6 py-2 bg-blue-600 text-white"> Saved excuses: </div>
        {excuses && <ExcuseList excuses={excuses} />}
      </div>
    </div>
  )
}

const ExcuseList = (props) => { 
  return props.excuses?.length ?  props.excuses.map((excuse, idx) => <Excuse excuse={excuse} key={idx} />) : 'No saved excuses'
}

const Excuse = ({ excuse }) => {
  return (
    <div className="px-6 py-2">
      {excuse.text}
    </div>
  )
}

export default MainPage
```

Our page consists of three components. `MainPage`, `ExcuseList` and `Excuse`. It may seem at first that this file is pretty complex. It’s not, so let’s look a bit closer. 

`Excuse` is just a div with an excuse text, `ExcuseList` checks if there are any excuses. If the list is empty – show a message `No saved excuses`. In other case – excuses will be displayed. 

`MainPage` contains info about the current excuses and the list of already saved excuses. Two button click handlers are `handleGetExcuse` and `handleSaveExcuse`. Plus, the markup itself with some Tailwind flavor. 

