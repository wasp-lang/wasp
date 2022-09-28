---
title: Adding operations
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Now you'll need to create two files: `actions.js` and `queries.js` in the `ext` folder.

Let’s add `saveExcuse()` action to our `actions.js` file. This action will save the text of our excuse to the database. 

```js title=".../ext/actions.js | Defining an action"
export const saveExcuse = async (excuse, context) => {
  return context.entities.Excuse.create({
    data: { text: excuse.text }
  })
}
```

Then we need to create two queries in the `queries.js` file. First, one `getExcuse` will call an external API and fetch a new excuse. The second one, named `getAllSavedExcuses`, will pull all the excuses we’ve saved to our database. 

```js title=".../ext/queries.js | Defining queries"
import axios from 'axios';

export const getExcuse = async () => {
  const response = await axios.get('https://api.devexcus.es/')
  return response.data
}

export const getAllSavedExcuses = async (_args, context) => {
  return context.entities.Excuse.findMany()
}
```

That’s it! We finished our back-end. 🎉 Now, let’s use those queries/actions on our UI.
