---
title: Modifying main.wasp file
---

import useBaseUrl from '@docusaurus/useBaseUrl';

First and foremost, we need to add some dependencies and introduce operations to our project. We’ll add Tailwind to make our UI prettier and Axios for making API requests. 

Also, we’ll declare a database entity called `Excuse`, queries, and action. The `Excuse` entity consists of the entity’s ID and the text. 

`Queries` are here when we need to fetch/read something, while `actions` are here when we need to change/update data. Both query and action declaration consists of two lines – a reference to the file that contains implementation and a data model to operate on. You can find more info [in the docs section below](/docs/language/features#queries-and-actions-aka-operations). So let’s proceed. 

Please, add this section to the `main.wasp` file's `app` section. 

```js title="main.wasp | Adding dependencies"
  head: [
    "<script src='https://cdn.tailwindcss.com'></script>"
  ],

  dependencies: [                                          
    ("axios", "^0.21.1")
  ]
```

Then add Excuse entity to the bottom of the file. Also you'll need to define queries and an action.

```js title="main.wasp | Defining Excuse entity, queries and action"
entity Excuse {=psl                                          
    id          Int     @id @default(autoincrement())
    text        String
psl=}

query getExcuse {                                           
  fn: import { getExcuse } from "@ext/queries.js",
  entities: [Excuse]
}

query getAllSavedExcuses {                                  
  fn: import { getAllSavedExcuses } from "@ext/queries.js",
  entities: [Excuse]
}

action saveExcuse {                                         
  fn: import { saveExcuse } from "@ext/actions.js",
  entities: [Excuse]
}
```
The resulting `main.wasp` file should look like this:

```js title="main.wasp | Final result"

// Main declaration, defines a new web app.
app ItWaspsOnMyMachine {

  // Used as a browser tab title.                                  
  title: "It Wasps On My Machine",

  head: [
    // Adding Tailwind to make our UI prettier
    "<script src='https://cdn.tailwindcss.com'></script>"
  ],

  dependencies: [ 
    // Adding Axios for making HTTP requests                                          
    ("axios", "^0.21.1")
  ]
}

// Render page MainPage on url `/` (default url).
route RootRoute { path: "/", to: MainPage }                 

// ReactJS implementation of our page located in `ext/MainPage.js` as a default export
page MainPage {                                             
  component: import Main from "@ext/MainPage.js"
}

// Prisma database entity
entity Excuse {=psl                                          
    id          Int     @id @default(autoincrement())
    text        String
psl=}

// Query declaration to get a new excuse
query getExcuse {                                           
  fn: import { getExcuse } from "@ext/queries.js",
  entities: [Excuse]
}

// Query declaration to get all excuses
query getAllSavedExcuses {                                  
  fn: import { getAllSavedExcuses } from "@ext/queries.js",
  entities: [Excuse]
}

// Action to save current excuse
action saveExcuse {                                         
  fn: import { saveExcuse } from "@ext/actions.js",
  entities: [Excuse]
}
```

Perfect! We've set up all the infrastructure. Now let's add some logic. 
