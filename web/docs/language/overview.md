---
title: Overview
---

Wasp is a declarative language that recognizes web application-specific terms (e.g. *page* or *route*) as
words (types) of the language.

The basic idea is that the higher-level overview of an app (e.g. pages, routes, database model, ...) is defined in `*.wasp` files (for now just one), while the specific parts (web components, back-end queries, ...) are implemented in specific non-wasp technologies (React, NodeJS, Prisma) and then referenced in the `*.wasp` files.

Basic structure of a Wasp project is:
- `*.wasp` file
- The `src/` folder -> Contains non-wasp code (JS, CSS, ...). You can structure it however you want, as long as you put it somewhere inside the correct subfolder:
  - The `src/server` folder - Contains your server code (i.e., executed by Node JS).
  - The `src/client` folder - Contains your client code (i.e., executed by JS in user's browsers).
  - The `src/shared` folder - Contains the code you want to share between the server and the client (e.g., utility functions).

When referencing code from `src/server` in your `*.wasp` file, you do it as `@server/relative/path/of/file/in/the/server/dir`.

When referencing code from `src/client` in your `*.wasp` file, you do it as `@client/relative/path/of/file/in/the/client/dir`.

You can't reference shared code inside the Wasp file, but you can import and use it in all code that lives in `src/client` or `src/server`. Use a relative import to do this. For example, the file `src/server/something.js` can import a shared function from `src/shared/utilities.js` like this:
```js
import someFunction from '../shared/utilities.js'

// ...
```


# Simple example
We're omitting all pregenerated none-code files Wasp needs to function (e.g., `jsconfig.json` and `.wasproot`) and are focusing only on the files you would write yourself:
```yaml
TodoApp/
  - main.wasp
  - src/
      - server/
        - operations.js
      - client/
        - pages/
          - Main.js
      - shared/
```

```css title="main.wasp"
app todoApp {
  wasp: {
    version: "^0.6.0"
  },

  title: "ToDo App"
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import Main from "@client/pages/Main"
}

query getTasks {
  fn: import { getTasks } from "@server/operations.js",
  entities: [Task]
}

action createTask {
  fn: import { createTask } from "@server/operations.js",
  entities: [Task]
}

entity Task {=psl
  id          Int     @id @default(autoincrement())
  description String
  isDone      Boolean @default(false)
psl=}
```

You can check out a full working example [here](https://github.com/wasp-lang/wasp/tree/main/waspc/examples/todoApp).

In the following sections each of the basic language features is explained. 
