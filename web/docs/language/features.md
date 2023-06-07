---
title: Features
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
import SendingEmailsInDevelopment from '../_sendingEmailsInDevelopment.md'

## App

There can be only one declaration of `app` type per Wasp project.
It serves as a starting point and defines global properties of your app.

```wasp
app todoApp {
  wasp: {
    version: "^0.6.0"
  },
  title: "ToDo App",
  head: [  // optional
    "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap\" />"
  ]
}
```

### Fields

#### `wasp: dict` (required)
Wasp compiler configuration. It is a dictionary with a single field:
-  `version: string` (required) - version declares the compatible Wasp versions for the app. It should contain a valid [SemVer range](https://github.com/npm/node-semver#ranges).

:::info
For now, the version field only supports caret ranges (i.e., `^x.y.z`). Support for the full specification will come in a future version of Wasp
:::

#### `title: string` (required)
Title of your app. It will be displayed in the browser tab, next to the favicon.

#### `head: [string]` (optional)
Head of your HTML Document. Your app's metadata (styles, links, etc) can be added here.

#### `auth: dict` (optional)
Authentication and authorization configuration.
Check [`app.auth`](/docs/language/features#authentication--authorization) for more details.

#### `client: dict` (optional)
Client configuration.
Check [`app.client`](/docs/language/features#client-configuration) for more details.

#### `server: dict` (optional)
Server configuration.
Check [`app.server`](/docs/language/features#server-configuration) for more details.

#### `db: dict` (optional)
Database configuration.
Check [`app.db`](/docs/language/features#database-configuration) for more details.

#### `dependencies: [(string, string)]` (optional)
List of dependencies (external libraries).
Check [`app.dependencies`](/docs/language/features#dependencies) for more details.

#### `emailSender: dict` (optional)
Email sender configuration.
Check [`app.emailSender`](/docs/language/features#email-sender) for more details.

## Page

`page` declaration is the top-level layout abstraction. Your app can have multiple pages.

```wasp
page MainPage {
  component: import Main from "@client/pages/Main",
  authRequired: false  // optional
}
```

Normally you will also want to associate `page` with a `route`, otherwise it won't be accessible in the app.

### Fields

#### `component: ClientImport` (required)
Import statement of the React element that implements the page component.

#### `authRequired: bool` (optional)
Can be specified only if [`app.auth`](/docs/language/features#authentication--authorization) is defined.

If set to `true`, only authenticated users will be able to access this page. Unauthenticated users will be redirected to a route defined by `onAuthFailedRedirectTo` property within `app.auth`.

If `authRequired` is set to `true`, the React component of a page (specified by `component` property) will be provided `user` object as a prop.

Check out this [section of our Todo app tutorial](/docs/tutorials/todo-app/06-auth#updating-main-page-to-check-if-user-is-authenticated) for an example of usage.

## Route

`route` declaration provides top-level routing functionality in Wasp.

```wasp
route AboutRoute { path: "/about", to: AboutPage }
```

### Fields

#### `path: string` (required)
URL path of the route. Route path can be parametrised and follows the same conventions as
[React Router](https://reactrouter.com/web/).

#### `to: page` (required)
Name of the `page` to which the path will lead.
Referenced page must be defined somewhere in `.wasp` file.

### Example - parametrised URL path
```wasp
route TaskRoute { path: "/task/:id", to: TaskPage }
```
For details on URL path format check [React Router](https://reactrouter.com/web/)
documentation.

### Accessing route parameters in a page component

Since Wasp under the hood generates code with [React Router](https://reactrouter.com/web/),
the same rules apply when accessing URL params in your React components. Here is an example just to get you
started:

```wasp title="todoApp.wasp"
// ...
route TaskRoute { path: "/task/:id", to: TaskPage }
page TaskPage {
  component: import Task from "@client/pages/Task"
}
```

```jsx title="pages/Task.js"
import React from 'react'

const Task = (props) => {
  return (
    <div>
      I am showing a task with id: {props.match.params.id}.
    </div>
  )
}

export default Task
```
### Navigating between routes

Navigation can be performed from the React code via `<Link/>` component, also using the functionality of
[React Router](https://reactrouter.com/web/):

```wasp title="todoApp.wasp"
// ...
route HomeRoute { path: "/home", to: HomePage }
page HomePage {
  component: import Home from "@client/pages/Home"
}
```

```jsx title="src/client/pages/OtherPage.js"
import React from 'react'
import { Link } from "react-router-dom"

const OtherPage = (props) => {
  return (
    <Link to="/home">Go to homepage</Link>
  )
}
```

## Entity

`entity` declaration represents a database model.
Wasp uses [Prisma](https://www.prisma.io/) to implement database functionality and currently provides only a thin layer above it.

Each `Entity` declaration corresponds 1-to-1 to Prisma data model and is defined in a following way:

```wasp
entity Task {=psl
    id          Int     @id @default(autoincrement())
    description String
    isDone      Boolean @default(false)
psl=}
```

### `{=psl ... psl=}: PSL`
Definition of entity fields in *Prisma Schema Language* (PSL). See
[here for intro and examples](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-schema)
and [here for a more exhaustive language specification](https://github.com/prisma/specs/tree/master/schema).

### Using Entities

Entity-system in Wasp is based on [Prisma](http://www.prisma.io), and currently Wasp provides only a thin layer
on top of it. The workflow is as follows:

1. Wasp developer creates/updates some of the entities in `.wasp` file.
2. Wasp developer runs `wasp db migrate-dev`.
3. Migration data is generated in `migrations/` folder (and should be commited).
4. Wasp developer uses Prisma JS API to work with the database when in Operations.

#### Using Entities in Operations

Most of the time in Wasp you will be working with entities in the context of Operations (Queries & Actions), so check their part of docs for more info on how to use entities in Operations.

#### Using Entities directly

If needed, you can also interact with entities directly via [Prisma Client(https://www.prisma.io/docs/concepts/components/prisma-client/crud) (although we recommend using them via injected `entities` when in Operations).

To import Prisma Client in your Wasp server code, do `import prismaClient from '@wasp/dbClient'`.

## Queries and Actions (aka Operations)

In Wasp, the client and the server interact with each other through Operations.
Wasp currently supports two kinds of Operations: **Queries** and **Actions**.

### Query

Queries are used to fetch data from the server. They do not modify the server's state.

Queries are implemented in NodeJS and executed within the server's context.
Wasp generates the code that lets you call the Query from anywhere in your code (client or server) using the same interface.
In other words, you won't have to worry about building an HTTP API for the Query, handling the request on the server, or even handling and caching the responses on the client.
Instead, simply focus on the business logic inside your Query and let Wasp take care of the rest!

To create a Wasp Query, you must:
1. Define the Query's NodeJS implementation
2. Declare the Query in Wasp using the `query` declaration

After completing these two steps, you'll be able to use the Query from any point in your code.


#### Defining the Query's NodeJS implementation
The Query's implementation is a NodeJS function that takes two arguments (it can be an `async` function but doesn't have to).
Since both arguments are positional, you can name the parameters however you want, but we'll stick with `args` and `context`:
1. `args`:  An object containing all the arguments (i.e., payload) **passed to the Query by the caller** (e.g., filtering conditions).
Take a look at [the examples of usage](#using-the-query) to see how to pass this object to the Query.
3. `context`: An additional context object **injected into the Query by Wasp**. This object contains user session information, as well as information about entities. The examples here won't use the context for simplicity purposes. You can read more about it in the [section about using entities in queries](#using-entities-in-queries).

Here's an example of three simple Queries:
```js title="src/server/queries.js"
// our "database"
const tasks = [
  { id: 1, description: "Buy some eggs", isDone: true },
  { id: 2, description: "Make an omelette", isDone: false },
  { id: 3, description: "Eat breakfast", isDone: false }
]

// You don't need to use the arguments if you don't need them
export const getAllTasks = () => {
  return tasks;
}

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks = (args) => {
  const { isDone } = args;
  return tasks.filter(task => task.isDone === isDone)
}

// Query implementations can be async functions and use await.
export const getTasksWithDelay = async () => {
  const result = await sleep(1000)
  return tasks
}
```

#### Declaring a Query in Wasp
After implementing your Queries in NodeJS, all that's left to do before using them is tell Wasp about it!
You can easily do this with the `query` declaration, which supports the following fields:
- `fn: ServerImport` (required) - The import statement of the Query's NodeJs implementation.
- `entities: [Entity]` (optional) - A list of entities you wish to use inside your Query.
We'll leave this option aside for now. You can read more about it [here](#using-entities-in-queries).

Wasp Queries and their implementations don't need to (but can) have the same name, so we will keep the names different to avoid confusion.
With that in mind, this is how you might declare the Queries that use the implementations from the previous step:
```wasp title="pages/main.wasp"
// ...

// Again, it most likely makes sense to name the Wasp Query after
// its implementation. We're changing the name to emphasize the difference.

query fetchAllTasks {
  fn: import { getAllTasks } from "@server/queries.js"
}

query fetchFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js"
}
```

After declaring a NodeJS function as a Wasp Query, two crucial things happen:
- Wasp **generates a client-side JavaScript function** that shares its name with the Query (e.g., `fetchFilteredTasks`).
This function takes a single optional argument - an object containing any serializable data you wish to use inside the Query.
Wasp will pass this object to the Query's implementation as its first positional argument (i.e., `args` from the previous step).
Such an abstraction works thanks to an HTTP API route handler Wasp generates on the server, which calls the Query's NodeJS implementation under the hood.
- Wasp **generates a server-side NodeJS function** that shares its name with the Query. This function's interface is identical to the client-side function from the previous point.

Generating two such functions ensures a uniform calling interface across the entire app (both client and server).


#### Using the Query
To use the Query, you can import it from `@wasp` and call it directly. As mentioned, the usage is the same regardless of whether you're on the server or the client:
```javascript
import fetchAllTasks from '@wasp/queries/fetchAllTasks.js'
import fetchFilteredTasks from '@wasp/queries/fetchFilteredTasks.js'

// ...

const allTasks = await fetchAllTasks();
const doneTasks = await fetchFilteredTasks({isDone: true})
```

**NOTE**: Wasp will not stop you from importing a Query's NodeJS implementation from `./queries.js` and calling it directly. However, we advise against this, as you'll lose all the useful features a Wasp Query provides (e.g., entity injection).

#### The `useQuery` hook
When using Queries on the client, you can make them reactive with the help of the `useQuery` hook.
This hook comes bundled with Wasp and is a thin wrapper around the `useQuery` hook from [_react-query_](https://github.com/tannerlinsley/react-query).

Wasp's `useQuery` hook accepts three arguments:
- `queryFn` (required): A Wasp query declared in the previous step or, in other words, the client-side query function generated by Wasp based on a `query` declaration.
- `queryFnArgs` (optional): The arguments object (payload) you wish to pass into the Query. The Query's NodeJS implementation will receive this object as its first positional argument.
- `options` (optional): A _react-query_ `options` object. Use this to change
  [the default
  behaviour](https://react-query.tanstack.com/guides/important-defaults) for
  this particular query. If you want to change the global defaults, you can do
  so in the [client setup function](#overriding-default-behaviour-for-queries).

Wasp's `useQuery` hook behaves mostly the same as [_react-query_'s `useQuery` hook](https://react-query.tanstack.com/docs/api#usequery), the only difference being in not having to supply the key (Wasp does this automatically under the hood).

Here's an example of calling the Queries using the `useQuery` hook:
```jsx
import React from 'react'
import { useQuery } from '@wasp/queries'

import fetchAllTasks from '@wasp/queries/fetchAllTasks'
import fetchFilteredTasks from '@wasp/queries/fetchFilteredTasks'

const MainPage = () => {
  const {
    data: allTasks,
    error: error1
  } = useQuery(fetchAllTasks)

  const {
    data: doneTasks,
    error: error2
  } = useQuery(fetchFilteredTasks, { isDone: true })

  return (
    <div>
        <h2>All Tasks</h2>
        {allTasks ? allTasks.map(task => <Task key={task.id} {...task}/>) : error1}

        <h2>Finished Tasks</h2>
        {doneTasks ? doneTasks.map(task => <Task key={task.id} {...task}/>) : error2}
    </div>
  )
}

const Task = ({ description, isDone }) => {
  return (
    <div>
        <p><strong>Description: </strong>{ description }</p>
        <p><strong>Is done: </strong>{ isDone ? 'Yes' : 'No' }</p>
    </div>
  )
}


export default MainPage
```

#### Error Handling
For security reasons, all exceptions thrown in the Query's NodeJS implementation are sent to the client as responses with the HTTP status code `500`, with all other details removed.
Hiding error details by default helps against accidentally leaking possibly sensitive information over the network.

If you do want to pass additional error information to the client, you can construct and throw an appropriate `HttpError` in your NodeJS Query function:
```js title=src/server/queries.js
import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  const statusCode = 403
  const message = 'You can\'t do this!'
  const data = { foo: 'bar' }
  throw new HttpError(statusCode, message, data)
}
```

If the status code is `4xx`, the client will receive a response object with the corresponding `.message` and `.data` fields and rethrow the error (with these fields included).
To prevent information leakage, the server won't forward these fields for any other HTTP status codes.

#### Using Entities in Queries
In most cases, resources used in Queries will be [Entities](#entity).
To use an Entity in your Query, add it to the query declaration in Wasp:

```wasp {4,9} title="main.wasp"

query fetchAllTasks {
  fn: import { getAllTasks } from "@server/queries.js",
  entities: [Task]
}

query fetchFilteredTasks {
  fn: import { getFilteredTasks } from "@server/queries.js",
  entities: [Task]
}
```

Wasp will inject the specified Entity into the Query's `context` argument, giving you access to the Entity's Prisma API:
```js title="src/server/queries.js"
export const getAllTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}

export const getFilteredTasks = async (args, context) => {
  return context.entities.Task.findMany({
    where: { isDone: args.isDone }
  })
}
```

The object `context.entities.Task` exposes `prisma.task` from [Prisma's CRUD API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).


### Action

Actions are very similar to Queries. So similar, in fact, we will only list the differences:
1. They can (and most often should) modify the server's state, while Queries are only allowed to read it.
2. Actions don't need to be reactive so you can call them directly. Still, Wasp does provide a `useAction` React hook for adding extra behavior to the Action (e.g., optimistic updates).
Read more about the [`useAction` hook](#the-useaction-hook) below.
3. `action` declarations in Wasp are mostly identical to `query` declarations. The only difference is in the declaration's name.

Here's an implementation of a simple Action:

```js title=src/server/actions.js
export const sayHi = async () => {
  console.log('The client said Hi!')
}
```
Its corresponding declaration in Wasp:

```wasp title="main.wasp"
// ...

action sayHi {
  fn: import { sayHi } from "@server/actions.js"
}
```
And an example of how to import and call the declared Action:

```js
import sayHi from '@wasp/actions/sayHi'

// ...

sayHi()
```

Here's an example on how you might define a less contrived Action.
```js title=src/server/actions.js
// ...
export const updateTaskIsDone = ({ id, isDone }, context) => {
  return context.entities.Task.update({
    where: { id },
    data: { isDone }
  })
}
```
```wasp title=main.wasp
action updateTaskIsDone {
  fn: import { updateTaskIsDone } from "@server/actions.js",
  entities: [Task]
}
```

And here is how you might use it:
```jsx {4,18} title=src/client/pages/Task.js
import React from 'react'
import { useQuery } from '@wasp/queries'
import fetchTask from '@wasp/queries/fetchTask'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'

const TaskPage = ({ id }) => {
  const { data: task } = useQuery(fetchTask, { id })

  if (!task) {
    return <h1>"Loading"</h1>
  }

  const { description, isDone } = task
  return (
    <div>
      <p><strong>Description: </strong>{description}</p>
      <p><strong>Is done: </strong>{isDone ? 'Yes' : 'No'}</p>
      <button onClick={() => updateTaskIsDone({ id, isDone: !isDone })}>
        Mark as {task.isDone ? 'undone' : 'done'}
      </button>
    </div>
  )
}
```

#### The `useAction` hook
When using Actions in components, you can enhance them with the help of the `useAction` hook. This hook comes bundled with Wasp and decorates Wasp Actions.
In other words, the hook returns a function whose API matches the original Action while also doing something extra under the hood (depending on how you configure it).

The `useAction` hook accepts two arguments:
- `actionFn` (required) - The Wasp Action (i.e., the client-side query function generated by Wasp based on a query declaration) you wish to enhance.
- `actionOptions` (optional) - An object configuring the extra features you want to add to the given Action. While this argument is technically optional, there is no point in using the `useAction` hook without providing it (it would be the same as using the Action directly). The Action options object supports the following fields:
  - `optimisticUpdates` (optional) - An array of objects where each object defines an [optimistic update](https://stackoverflow.com/a/33009713) to perform on the query cache. To define an optimistic update, you must specify the following properties:
    - `getQuerySpecifier` (required) - A function returning the query specifier (i.e., a value used to address the query you want to update). A query specifier is an array specifying the query function and arguments. For example, to optimistically update the query used with `useQuery(fetchFilteredTasks, {isDone: true }]`, your `getQuerySpecifier` function would have to return the array `[fetchFilteredTasks, { isDone: true}]`. Wasp will forward the argument you pass into the decorated Action to this function (i.e., you can use the properties of the added/change item to address the query).
    - `updateQuery` (required) - The function used to perform the optimistic update. It should return the desired state of the cache. Wasp will call it with the following arguments:
      - `item` - The argument you pass into the decorated Action.
      - `oldData` - The currently cached value for the query identified by the specifier.

**NOTE:** The `updateQuery` function must be a pure function. It must return the desired cache value identified by the `getQuerySpecifier` function and _must not_ perform any side effects. Also, make sure you only update the query caches affected by your action causing the optimistic update (Wasp cannot yet verify this). Finally, your implementation of the `updateQuery` function should work correctly regardless of the state of `oldData` (e.g., don't rely on array positioning). If you need to do something else during your optimistic update, you can directly use _react-query_'s lower-level API (read more about it [here](#advanced-usage)).

Here's an example showing how to configure the Action from the previous example to perform an optimistic update:
```jsx {3,9,10,11,12,13,14,15,16,27} title=src/client/pages/Task.js
import React from 'react'
import { useQuery } from '@wasp/queries'
import { useAction } from '@wasp/actions'
import fetchTask from '@wasp/queries/fetchTask'
import updateTaskIsDone from '@wasp/actions/updateTaskIsDone'

const TaskPage = ({ id }) => {
  const { data: task } = useQuery(fetchTask, { id })
  const updateTaskIsDoneOptimistically = useAction(updateTaskIsDone, {
    optimisticUpdates: [
      {
        getQuerySpecifier: ({ id }) => [fetchTask, { id }],
        updateQuery: ({ isDone }, oldData) => ({ ...oldData, isDone })
      }
    ]
  })

  if (!task) {
    return <h1>"Loading"</h1>
  }

  const { description, isDone } = task
  return (
    <div>
      <p><strong>Description: </strong>{description}</p>
      <p><strong>Is done: </strong>{isDone ? 'Yes' : 'No'}</p>
      <button onClick={() => updateTaskIsDoneOptimistically({ id, isDone: !isDone })}>
        Mark as {task.isDone ? 'undone' : 'done'}
      </button>
      <div>
        <Link to="/">Back to main page</Link>
      </div>
    </div>
  )
}

export default TaskPage
```
#### Advanced usage
The `useAction` hook currently only supports specifying optimistic updates. You can expect more features in future versions of Wasp.

Wasp's optimistic update API is deliberately small and focuses exclusively on updating Query caches (as that's the most common use case). You might need an API that offers more options or a higher level of control. If that's the case, instead of using Wasp's `useAction` hook, you can use _react-query_'s `useMutation` hook and directly work with [their low-level API](https://tanstack.com/query/v4/docs/guides/optimistic-updates?from=reactQueryV3&original=https://react-query-v3.tanstack.com/guides/optimistic-updates).

If you decide to use _react-query_'s API directly, you will need access to the Query's cache key. Wasp internally uses this key but abstracts it from the programmer. Still, you can easily obtain it by accessing the `queryCacheKey` property on a Query:
```js
import { fetchTasks } from '@wasp/queries'

const queryKey = fetchTasks.queryCacheKey
```

### Cache Invalidation
One of the trickiest parts of managing a web app's state is making sure the data returned by the queries is up to date.
Since Wasp uses _react-query_ for Query management, we must make sure to invalidate Queries (more specifically, their cached results managed by _react-query_) whenever they become stale.

It's possible to invalidate the caches manually through several mechanisms _react-query_ provides (e.g., refetch, direct invalidation).
However, since manual cache invalidation quickly becomes complex and error-prone, Wasp offers a quicker and a more effective solution to get you started: **automatic Entity-based Query cache invalidation**.
Because Actions can (and most often do) modify the state while Queries read it, Wasp invalidates a Query's cache whenever an Action that uses the same Entity is executed.

For example, let's assume that Action `createTask` and Query `getTasks` both use Entity `Task`. If `createTask` is executed, `getTasks`'s cached result may no longer be up-to-date.
Wasp will therefore invalidate it, making `getTasks` refetch data from the server, bringing it up to date again.

In practice, this means that Wasp keeps the queries "fresh" without requiring you to think about cache invalidation.

On the other hand, this kind of automatic cache invalidation can become wasteful (some updates might not be necessary) and will only work for Entities. If that's an issue, you can use the mechanisms provided by _react-query_ for now, and expect more direct support in Wasp for handling those use cases in a nice, elegant way.

If you wish to optimistically set cache values after perfomring an action, you can do so using [optimistic updates](https://stackoverflow.com/a/33009713). Configure them using Wasp's [useAction hook](#the-useaction-hook). This is currently the only manual cache invalidation mechanism Wasps supports natively. For everything else, you can always rely on _react-query_.

### Prisma Error Helpers
In your Operations, you may wish to handle general Prisma errors with HTTP-friendly responses. We have exposed two helper functions, `isPrismaError`, and `prismaErrorToHttpError`, for this purpose. As of now, we convert two specific Prisma errors (which we will continue to expand), with the rest being `500`. See the [source here](https://github.com/wasp-lang/wasp/blob/main/waspc/e2e-test/test-outputs/waspMigrate-golden/waspMigrate/.wasp/out/server/src/utils.js).

#### `import statement`:
```js
import { isPrismaError, prismaErrorToHttpError } from '@wasp/utils.js'
```

##### Example of usage:
```js
try {
  await context.entities.Task.create({...})
} catch (e) {
  if (isPrismaError(e)) {
    throw prismaErrorToHttpError(e)
  } else {
    throw e
  }
}
```

## APIs

In Wasp, the default client-server interaction mechanism is through [Operations](#queries-and-actions-aka-operations). However, if you need a specific URL method/path, or a specific response, Operations may not be suitable for you. For these cases, you can use an `api`! Best of all, they should look and feel very familiar.

### API

APIs are used to tie a JS function to an HTTP (method, path) pair. They are distinct from Operations, and have no client-side helpers (like `useQuery`).

To create a Wasp API, you must:
1. Define the APIs NodeJS implementation
2. Declare the API in Wasp using the `api` declaration

After completing these two steps, you'll be able to call the API from client code (via our Axios wrapper), or from the outside world.

:::note
In order to leverage the benefits of TypeScript and use types in your NodeJS implementation (step 1), you must add your `api` declarations to your `.wasp` file (step 2) _and_ compile the Wasp project. This will enable the Wasp compiler to generate any new types based on your `.wasp`file definitions for use in your implementation files.
:::

#### Defining the APIs NodeJS implementation
An API should be implemented as a NodeJS function that takes three arguments.
1. `req`:  Express Request object
2. `res`: Express Response object
3. `context`: An additional context object **injected into the API by Wasp**. This object contains user session information, as well as information about entities. The examples here won't use the context for simplicity purposes. You can read more about it in the [section about using entities in APIs](#using-entities-in-apis).

##### Simple API example
```ts title="src/server/apis.ts"
import { FooBar } from '@wasp/apis/types'

export const fooBar : FooBar = (req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override Wasp default CORS middleware.
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}
```

##### More complicated TypeScript example
Let's say you wanted to create some `GET` route that would take an email address as a param, and provide them the answer to "Life, the Universe and Everything." :) What would this look like in TypeScript?

```wasp title="main.wasp"
api fooBar {
  fn: import { fooBar } from "@server/apis.js",
  entities: [Task],
  httpRoute: (GET, "/foo/bar/:email")
}
```

```ts title="src/server/apis.ts"
import { FooBar } from '@wasp/apis/types'

export const fooBar: FooBar<
{ email: string }, // params
{ answer: number }  // response
> = (req, res, _context) => {
  console.log(req.params.email)
  res.json({ answer: 42 })
}
```

#### Declaring an API in Wasp
After implementing your APIs in NodeJS, all that's left to do before using them is tell Wasp about it!
You can easily do this with the `api` declaration, which supports the following fields:
- `fn: ServerImport` (required) - The import statement of the APIs NodeJs implementation.
- `httpRoute: (HttpMethod, string)` (required) - The HTTP (method, path) pair, where the method can be one of:
  - `ALL`, `GET`, `POST`, `PUT` or `DELETE`
  - and path is an Express path `string`.
- `entities: [Entity]` (optional) - A list of entities you wish to use inside your API.
We'll leave this option aside for now. You can read more about it [here](#using-entities-in-apis).
- `auth: bool` (optional) - If auth is enabled, this will default to `true` and provide a `context.user` object. If you do not wish to attempt to parse the JWT in the Authorization Header, you may set this to `false`.
- `middlewareConfigFn: ServerImport` (optional) - The import statement to an Express middleware config function for this API. See [the guide here](/docs/guides/middleware-customization#2-customize-api-specific-middleware).

Wasp APIs and their implementations don't need to (but can) have the same name. With that in mind, this is how you might declare the API that uses the implementations from the previous step:
```wasp title="pages/main.wasp"
// ...

api fooBar {
  fn: import { fooBar } from "@server/apis.js",
  httpRoute: (GET, "/foo/bar")
}
```

#### Using the API
To use the API externally, you simply call the endpoint using the method and path you used. For example, if your app is running at `https://example.com` then from the above you could issue a `GET` to `https://example/com/foo/callback` (in your browser, Postman, `curl`, another web service, etc.).

To use the API from your client, including with auth support, you can import the Axios wrapper from `@wasp/api` and invoke a call. For example:
```ts
import React, { useEffect } from 'react'
import api from '@wasp/api'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const Foo = () => {
  useEffect(() => {
    fetchCustomRoute()
  }, []);

  return (
    <>
      // ...
    </>
  )
}
```

#### Using Entities in APIs
In many cases, resources used in APIs will be [Entities](#entity).
To use an Entity in your API, add it to the `api` declaration in Wasp:

```wasp {3} title="main.wasp"
api fooBar {
  fn: import { fooBar } from "@server/apis.js",
  entities: [Task],
  httpRoute: (GET, "/foo/bar")
}
```

Wasp will inject the specified Entity into the APIs `context` argument, giving you access to the Entity's Prisma API:
```ts title="src/server/apis.ts"
import { FooBar } from '@wasp/apis/types'

export const fooBar : FooBar = (req, res, context) => {
  res.json({ count: await context.entities.Task.count() })
}

```

The object `context.entities.Task` exposes `prisma.task` from [Prisma's CRUD API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).

### `apiNamespace`

An `apiNamespace` is a simple declaration used to apply some `middlewareConfigFn` to all APIs under some specific path. For example:

```wasp title="main.wasp"
apiNamespace fooBar {
  middlewareConfigFn: import { fooBarNamespaceMiddlewareFn } from "@server/apis.js",
  path: "/foo/bar"
}
```

For more information about middleware configuration, please see: [Middleware Configuration](/docs/guides/middleware-customization)

## Jobs

If you have server tasks that you do not want to handle as part of the normal request-response cycle, Wasp allows you to make that function a `job` and it will gain some "superpowers." Jobs will:
  * persist between server restarts
  * can be retried if they fail
  * can be delayed until the future
  * can have a recurring schedule!

Some examples where you may want to use a `job` on the server include sending an email, making an HTTP request to some external API, or doing some nightly calculations.

### Job Executors

Job executors handle the scheduling, monitoring, and execution of our jobs.

Wasp allows you to choose which job executor will be used to execute a specific job that you define, which affects some of the finer details of how jobs will behave and how they can be further configured. Each job executor has its pros and cons, which we will explain in more detail below, so you can pick the one that best suits your needs.

Currently, Wasp supports only one type of job executor, which is `PgBoss`, but in the future, it will likely support more.

#### pg-boss

We have selected [pg-boss](https://github.com/timgit/pg-boss/) as our first job executor to handle the low-volume, basic job queue workloads many web applications have. By using PostgreSQL (and [SKIP LOCKED](https://www.2ndquadrant.com/en/blog/what-is-select-skip-locked-for-in-postgresql-9-5/)) as its storage and synchronization mechanism, it allows us to provide many job queue pros without any additional infrastructure or complex management.

:::info
Keep in mind that pg-boss jobs run alongside your other server-side code, so they are not appropriate for CPU-heavy workloads. Additionally, some care is required if you modify scheduled jobs. Please see pg-boss details below for more information.

<details>
  <summary>pg-boss details</summary>

  pg-boss provides many useful features, which can be found [here](https://github.com/timgit/pg-boss/blob/8.4.2/README.md).

  When you add pg-boss to a Wasp project, it will automatically add a new schema to your database called `pgboss` with some internal tracking tables, including `job` and `schedule`. pg-boss tables have a `name` column in most tables that will correspond to your `job` identifier. Additionally, these tables maintain arguments, states, return values, retry information, start and expiration times, and other metadata required by pg-boss.

  If you need to customize the creation of the pg-boss instance, you can set an environment variable called `PG_BOSS_NEW_OPTIONS` to a stringified JSON object containing [these initialization parameters](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#newoptions). **NOTE**: Setting this overwrites all Wasp defaults, so you must include database connection information as well.

  ##### pg-boss considerations
  - Wasp starts pg-boss alongside your web server's application, where both are simultaneously operational. This means that jobs running via pg-boss and the rest of the server logic (like Operations) share the CPU, therefore you should avoid running CPU-intensive tasks via jobs.
    - Wasp does not (yet) support independent, horizontal scaling of pg-boss-only applications, nor starting them as separate workers/processes/threads.
  - The job name/identifier in your `.wasp` file is the same name that will be used in the `name` column of pg-boss tables. If you change a name that had a `schedule` associated with it, pg-boss will continue scheduling those jobs but they will have no handlers associated, and will thus become stale and expire. To resolve this, you can remove the applicable row from the `schedule` table in the `pgboss` schema of your database.
    - If you remove a `schedule` from a job, you will need to do the above as well.
  - If you wish to deploy to Heroku, you need to set an additional environment variable called `PG_BOSS_NEW_OPTIONS` to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.
- https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js

</details>
:::

### Basic job definition and usage

To declare a `job` in Wasp, simply add a declaration with a reference to an `async` function, like the following:

```wasp title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@server/workers/bar.js"
  }
}
```

Then, in your [Operations](/docs/language/features#queries-and-actions-aka-operations) or [setupFn](/docs/language/features#setupfn-serverimport-optional) (or any other NodeJS code), you can submit work to be done:
```js
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

const submittedJob = await mySpecialJob.submit({ job: "args" })
console.log(await submittedJob.pgBoss.details())

// Or, if you'd prefer it to execute in the future, just add a .delay().
// It takes a number of seconds, Date, or ISO date string.
await mySpecialJob.delay(10).submit({ job: "args" })
```

And that is it! Your job will be executed by the job executor (pg-boss, in this case) as if you called `foo({ job: "args" })`.

Note that in our example, `foo` takes an argument, but this does not always have to be the case. It all depends on how you've implemented your worker function.

### Recurring jobs

If you have work that needs to be done on some recurring basis, you can add a `schedule` to your job declaration:

```wasp  {6-9} title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@server/workers/bar.js"
  },
  schedule: {
    cron: "0 * * * *",
    args: {=json { "job": "args" } json=} // optional
  }
}
```

In this example, you do _not_ need to invoke anything in JavaScript. You can imagine `foo({ job: "args" })` getting automatically scheduled and invoked for you every hour.

### Fully specified example
Both `perform` and `schedule` accept `executorOptions`, which we pass directly to the named job executor when you submit jobs. In this example, the scheduled job will have a `retryLimit` set to 0, as `schedule` overrides any similar property from `perform`. Lastly, we add an entity to pass in via the context argument to `perform.fn`.

```wasp
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@server/workers/bar.js",
    executorOptions: {
      pgBoss: {=json { "retryLimit": 1 } json=}
    }
  },
  schedule: {
    cron: "*/5 * * * *",
    args: {=json { "foo": "bar" } json=},
    executorOptions: {
      pgBoss: {=json { "retryLimit": 0 } json=}
    }
  },
  entities: [Task],
}
```

### Fields

#### `executor: JobExecutor` (required)
`PgBoss` is currently our only job executor, and is recommended for low-volume production use cases. It requires your `app.db.system` to be `PostgreSQL`.

####  `perform: dict` (required)

  - ##### `fn: ServerImport` (required)
  An `async` JavaScript function of work to be performed. Since Wasp executes jobs on the server, you must import it from `@server`. The function receives a first argument which may be passed when the job is called, as well as the context containing any declared entities as the second (this is passed automatically by Wasp). Here is a sample signature:

  ```js
  export async function foo(args, context) {
    // Can reference context.entities.Task, for example.
  }
  ```

  - ##### `executorOptions: dict` (optional)
  Executor-specific default options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. These can be overridden during invocation with `submit()` or in a `schedule`.

    - ##### `pgBoss: JSON` (optional)
    See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options).

#### `schedule: dict` (optional)

  - ##### `cron: string` (required)
  A 5-placeholder format cron expression string. See rationale for minute-level precision [here](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#scheduling).

  _If you need help building cron expressions, Check out_ <em>[Crontab guru](https://crontab.guru/#0_*_*_*_*).</em>

  - ##### `args: JSON` (optional)
  The arguments to pass to the `perform.fn` function when invoked.

  - ##### `executorOptions: dict` (optional)
  Executor-specific options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. The `perform.executorOptions` are the default options, and `schedule.executorOptions` can override/extend those.

    - ##### `pgBoss: JSON` (optional)
    See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options).

#### `entities: [Entity]` (optional)
A list of entities you wish to use inside your Job (similar to Queries and Actions).

### JavaScript API

#### Invocation
##### `import`

```js
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'
```

##### `submit(jobArgs, executorOptions)`
- ###### `jobArgs: JSON` (optional)
- ###### `executorOptions: JSON` (optional)

Submits a `job` to be executed by an executor, optionally passing in a JSON job argument your job handler function will receive, and executor-specific submit options.

```js
const submittedJob = await mySpecialJob.submit({ job: "args" })
```

##### `delay(startAfter)` (optional)
- ###### `startAfter: int | string | Date` (required)

Delaying the invocation of the job handler. The delay can be one of:
- Integer: number of seconds to delay. [Default 0]
- String: ISO date string to run at.
- Date: Date to run at.

```js
const submittedJob = await mySpecialJob.delay(10).submit({ job: "args" }, { "retryLimit": 2 })
```

#### Tracking
The return value of `submit()` is an instance of `SubmittedJob`, which minimally contains:
- `jobId`: A getter returning the UUID String ID for the job in that executor.
- `jobName`: A getter returning the name of the job you used in your `.wasp` file.
- `executorName`: A getter returning a Symbol of the name of the job executor.
  - For pg-boss, you can import a Symbol from: `import { PG_BOSS_EXECUTOR_NAME } from '@wasp/jobs/core/pgBoss/pgBossJob.js'` if you wish to compare against `executorName`.

There will also be namespaced, job executor-specific objects.

- For pg-boss, you may access: `pgBoss`
  - **NOTE**: no arguments are necessary, as we already applied the `jobId` in the available functions.
  - `details()`: pg-boss specific job detail information. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#getjobbyidid)
  - `cancel()`: attempts to cancel a job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#cancelid)
  - `resume()`: attempts to resume a canceled job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#resumeid)

## Dependencies

You can specify additional npm dependencies via `dependencies` field in `app` declaration, in following way:

```wasp
app MyApp {
  title: "My app",
  // ...
  dependencies: [
    ("redux", "^4.0.5"),
    ("react-redux", "^7.1.3")
  ]
}
```

You will need to re-run `wasp start` after adding a dependency for Wasp to pick it up.

**NOTE**: In current implementation of Wasp, if Wasp is already internally using certain npm dependency with certain version specified, you are not allowed to define that same npm dependency yourself while specifying different version.
If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp dictates exact versions of certain packages, so for example you can't choose version of React you want to use.
In the future, we will add support for picking any version you like, but we have not implemented that yet. Check [issue #59](https://github.com/wasp-lang/wasp/issues/59) to check out the progress or contribute.


## Authentication & Authorization

Wasp provides authentication and authorization support out-of-the-box. Enabling it for your app is optional and can be done by configuring the `auth` field of the `app` declaration:

```wasp
app MyApp {
  title: "My app",
  //...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {}, // use this or email, not both
      email: {}, // use this or usernameAndPassword, not both
      google: {},
      gitHub: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

//...
```

`app.auth` is a dictionary with following fields:

#### `userEntity: entity` (required)
Entity which represents the user.

#### `externalAuthEntity: entity` (optional)
Entity which associates a user with some external authentication provider. We currently offer support for Google and GitHub. See the sections on [Social Login Providers](#social-login-providers-oauth-20) for more info.

#### `methods: dict` (required)
List of authentication methods that Wasp app supports. Currently supported methods are:
* `usernameAndPassword`: authentication with a username and password. See [here](#username-and-password) for more.
* `email`: authentication with a email and password. See [here](#email-authentication) for more.
* `google`: authentication via Google accounts. See [here](#social-login-providers-oauth-20) for more.
* `gitHub`: authentication via GitHub accounts. See [here](#social-login-providers-oauth-20) for more.

#### `onAuthFailedRedirectTo: String` (required)
Path where an unauthenticated user will be redirected to if they try to access a private page (which is declared by setting `authRequired: true` for a specific page).
Check out this [section of our Todo app tutorial](/docs/tutorials/todo-app/06-auth#updating-main-page-to-check-if-user-is-authenticated) to see an example of usage.

#### `onAuthSucceededRedirectTo: String` (optional)
Path where a successfully authenticated user will be sent upon successful login/signup.
Default value is "/".

:::note
Automatic redirect on successful login only works when using the Wasp provided [`Signup` and `Login` forms](#high-level-api)
:::

### Username and Password

`usernameAndPassword` authentication method makes it possible to signup/login into the app by using a username and password.
This method requires that `userEntity` specified in `auth` contains `username: string` and `password: string` fields:

```wasp
app MyApp {
  title: "My app",
  //...

  auth: {
    userEntity: User,
    methods: {
      usernameAndPassword: {},
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}

// Wasp requires the userEntity to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
psl=}
```

We provide basic validations out of the box, which you can customize as shown below. Default validations are:
- `username`: non-empty
- `password`: non-empty, at least 8 characters, and contains a number

Note that `username`s are stored in a case-sensitive manner.

#### High-level API

The quickest way to get started is by using the following API generated by Wasp:
- Signup and Login forms at `@wasp/auth/forms/Signup` and `@wasp/auth/forms/Login` routes
  - For styling, these default authentication components have form classes associated for both login (`login-form`) and signup (`signup-form`). Additionally, they both share a common class (`auth-form`).
- `logout` function
- `useAuth()` React hook
**NOTE:** If the signup is successful, the Signup form will automatically log in the user.

Check our [Todo app tutorial](/docs/tutorials/todo-app/06-auth) to see how it works. See below for detailed specification of each of these methods.

#### Lower-level API

If you require more control in your authentication flow, you can achieve that in the following ways:
- If you don't want to use already generated Signup and Login forms and want to create your own, you can use `signup` and `login` function by invoking them from the client.
- If you want to execute custom code on the server during sign up, create your own sign up action which invokes Prisma client as `context.entities.[USER_ENTITY].create()` function, along with your custom code.

The code of your custom sign-up action would look like this (your user entity being `User` in this instance):
```js
export const signUp = async (args, context) => {
  // Your custom code before sign-up.
  // ...

  const newUser = context.entities.User.create({
    data: {
      username: args.username,
      password: args.password // password hashed automatically by Wasp! 🐝
    }
  })

  // Your custom code after sign-up.
  // ...
  return newUser
}
```

:::info
You don't need to worry about hashing the password yourself! Even when you are using Prisma's client directly and calling `create()` with a plain-text password, Wasp's middleware takes care of hashing it before storing it in the database. An additional middleware also performs field validation.
:::

##### Customizing user entity validations

To disable/enable default validations, or add your own, you can modify your custom signUp function like so:
```js
const newUser = context.entities.User.create({
  data: {
    username: args.username,
    password: args.password // password hashed automatically by Wasp! 🐝
  },
  _waspSkipDefaultValidations: false, // can be omitted if false (default), or explicitly set to true
  _waspCustomValidations: [
    {
      validates: 'password',
      message: 'password must contain an uppercase letter',
      validator: password => /[A-Z]/.test(password)
    },
  ]
})
```

:::info
Validations always run on `create()`, but only when the field mentioned in `validates` is present for `update()`. The validation process stops on the first `validator` to return false. If enabled, default validations run first and validate basic properties of both the `'username'` or `'password'` fields.
:::

#### Specification

#### `login()`
An action for logging in the user.
```js
login(username, password)
```
:::info
When using the exposed `login()` function, make sure to implement your own redirect on successful login logic
:::

#### `username: string`
Username of the user logging in.

#### `password: string`
Password of the user logging in.

#### `import statement`:
```js
import login from '@wasp/auth/login.js'
```
Login is a regular action and can be used directly from the frontend.


#### `signup()`
An action for signing up the user. This action does not log in the user, you still need to call `login()`.

```js
signup(userFields)
```
#### `userFields: object`
Auth-related fields (either `username` or `email` and `password``) of the user entity which was declared in `auth`.

:::info
Wasp only stores the auth-related fields of the user entity. Adding extra fields to `userFields` will not have any effect.

If you need to add extra fields to the user entity, we suggest doing it in a separate step after the user logs in for the first time.
:::

#### `import statement`:
```js
import signup from '@wasp/auth/signup.js'
```
Signup is a regular action and can be used directly from the frontend.

#### `logout()`
An action for logging out the user.
```js
logout()
```

#### `import statement`:
```js
import logout from '@wasp/auth/logout.js'
```

##### Example of usage:
```jsx
import logout from '@wasp/auth/logout.js'

const SignOut = () => {
  return (
    <button onClick={logout}>Logout</button>
  )
}
```

#### Updating a user's password
If you need to update user's password, you can do it safely via Prisma client, e.g. within an action:
```js
export const updatePassword = async (args, context) => {
  return context.entities.User.update({
    where: { id: args.userId },
    data: {
      password: 'New pwd which will be hashed automatically!'
    }
  })
}
```
You don't need to worry about hashing the password yourself - if you have an `auth` declaration
in your `.wasp` file, Wasp already set a middleware on Prisma that makes sure whenever password
is created or updated on the user entity, it is also hashed before it is stored to the database.

### Email authentication

:::info 
We have written a step-by-step guide on how to set up the e-mail authentication with Wasp's included Auth UI. 

Read more in the [email authentication guide](/docs/guides/email-auth).
:::

:::warning
If a user signs up with Google or Github (and you set it up to save their social provider e-mail info on the `User` entity), they'll be able to reset their password and login with e-mail and password.

If a user signs up with the e-mail and password and then tries to login with a social provider (Google or Github), they won't be able to do that.

In the future, we will lift this limitation and enable smarter merging of accounts.
:::

`email` authentication method makes it possible to signup/login into the app by using an e-mail and a password.

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...

  auth: {
    userEntity: User,
    methods: {
      email: {
        // we'll deal with `email` below
      },
    },
    onAuthFailedRedirectTo: "/someRoute"
  },
  // ...
}

// Wasp requires the userEntity to have at least the following fields
entity User {=psl
    id                        Int           @id @default(autoincrement())
    email                     String?       @unique
    password                  String?
    isEmailVerified           Boolean       @default(false)
    emailVerificationSentAt   DateTime?
    passwordResetSentAt       DateTime?
psl=}
```

This method requires that `userEntity` specified in `auth` contains:

- optional `email` field of type `String`
- optional `password` field of type `String`
- `isEmailVerified` field of type `Boolean` with a default value of `false`
- optional `emailVerificationSentAt` field of type `DateTime`
- optional `passwordResetSentAt` field of type `DateTime`

#### Fields in the `email` dict

```wasp title="main.wasp"
app MyApp {
  title: "My app",
  // ...

  auth: {
    userEntity: User,
    methods: {
      email: {
        fromField: {
          name: "My App",
          email: "hello@itsme.com"
        },
        emailVerification: {
          clientRoute: EmailVerificationRoute,
          getEmailContentFn: import { getVerificationEmailContent } from "@server/auth/email.js",
        },
        passwordReset: {
          clientRoute: PasswordResetRoute
          getEmailContentFn: import { getPasswordResetEmailContent } from "@server/auth/email.js",
        },
        allowUnverifiedLogin: false,
      },
    },
    onAuthFailedRedirectTo: "/someRoute"
  },
  // ...
}
```

##### `fromField: EmailFromField` (required)
`fromField` is a dict that specifies the name and e-mail address of the sender of the e-mails sent by Wasp. It is required to be defined. The object has the following fields:
- `name`: name of the sender (optional)
- `email`: e-mail address of the sender

##### `emailVerification: EmailVerificationConfig` (required)
`emailVerification` is a dict that specifies the e-mail verification process. It is required to be defined.

The object has the following fields:
- `clientRoute: Route`: a route that is used for the user to verify their e-mail address. (required)

Client route should handle the process of taking a token from the URL and sending it to the server to verify the e-mail address. You can use our `verifyEmail` action for that.

```js title="src/pages/EmailVerificationPage.jsx"
import { verifyEmail } from '@wasp/auth/email/actions';
...
await verifyEmail({ token });
```

Read on how to do it the easiest way with Auth UI in the [email authentication guide](/docs/guides/email-auth).

- `getEmailContentFn: ServerImport`: a function that returns the content of the e-mail that is sent to the user. (optional)

Defining `getEmailContentFn` can be done by defining a Javscript or Typescript file in the `server` directory.

```ts title="server/email.ts"
import { GetVerificationEmailContentFn } from '@wasp/types'

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: 'Verify your email',
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
})
```

##### `passwordReset: PasswordResetConfig` (required)
`passwordReset` is a dict that specifies the password reset process. It is required to be defined. The object has the following fields:
- `clientRoute: Route`: a route that is used for the user to reset their password. (required)

Client route should handle the process of taking a token from the URL and a new password from the user and sending it to the server.  You can use our `requestPasswordReset` and `resetPassword` actions to do that.

```js title="src/pages/ForgotPasswordPage.jsx"
import { requestPasswordReset } from '@wasp/auth/email/actions';
...
await requestPasswordReset({ email });
```

```js title="src/pages/PasswordResetPage.jsx"
import { resetPassword } from '@wasp/auth/email/actions';
...
await resetPassword({ password, token })
```

##### `allowUnverifiedLogin: bool`: a boolean that specifies whether the user can login without verifying their e-mail address. (optional)

It defaults to `false`. If `allowUnverifiedLogin` is set to `true`, the user can login without verifying their e-mail address, otherwise users will receive a `401` error when trying to login without verifying their e-mail address.


Read on how to do it the easiest way with Auth UI in the [email authentication guide](/docs/guides/email-auth).

- `getEmailContentFn: ServerImport`: a function that returns the content of the e-mail that is sent to the user. (optional)

Defining `getEmailContentFn` is done by defining a function that looks like this:

```ts title="server/email.ts"
import { GetPasswordResetEmailContentFn } from '@wasp/types'

export const getPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
  passwordResetLink,
}) => ({
  subject: 'Password reset',
  text: `Click the link below to reset your password: ${passwordResetLink}`,
  html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
})
```

#### Email sender for email authentication

We require that you define an `emailSender`, so that Wasp knows how to send e-mails. Read more about that [here](#email-sender).

#### Validations

We provide basic validations out of the box. The validations are:
- `email`: non-empty, valid e-mail address
- `password`: non-empty, at least 8 characters, and contains a number

Note that `email`s are stored in a case-insensitive manner.

:::info
You don't need to worry about hashing the password yourself! Even when you are using Prisma's client directly and calling `create()` with a plain-text password, Wasp's middleware takes care of hashing it before storing it in the database. An additional middleware also performs field validation.
:::



### Social Login Providers (OAuth 2.0)
Wasp allows you to easily add social login providers to your app.

The following is a list of links to guides that will help you get started with the currently supported providers:
- [GitHub](/docs/integrations/github)
- [Google](/docs/integrations/google)

When using Social Login Providers, Wasp gives you the following options:
- Default settings to get you started quickly
- UI Helpers to make it easy to add social login buttons and actions
- Override settings to customize the behavior of the providers

#### Default Settings


<Tabs>
<TabItem value="google" label="Google" default>

```wasp
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {},
    },
  }
```

<p>Add <code>google: &#123;&#125;</code> to your <code>auth.methods</code> dictionary to use it with default settings</p>
<p>By default, Wasp expects you to set two environment variables in order to use Google authentication:</p>
<ul>
  <li><code>GOOGLE_CLIENT_ID</code></li>
  <li><code>GOOGLE_CLIENT_SECRET</code></li>
</ul>
<p>These can be obtained in your Google Cloud Console project dashboard. See <a href="/docs/integrations/google">here</a> for a detailed guide.</p>

</TabItem>
<TabItem value="gitHub" label="GitHub">

```wasp
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      gitHub: {},
    },
  }
```

<p>Add <code>gitHub: &#123;&#125;</code> to your <code>auth.methods</code> dictionary to use it with default settings</p>
<p>By default, Wasp expects you to set two environment variables in order to use GitHub authentication:</p>
<ul>
  <li><code>GITHUB_CLIENT_ID</code></li>
  <li><code>GITHUB_CLIENT_SECRET</code></li>
</ul>
<p>These can be obtained in your GitHub project dashboard. See <a href="/docs/integrations/github">here</a> for a detailed guide.</p>

</TabItem>
</Tabs>

When a user signs in for the first time, if the `userEntity` has `username` and/or `password` fields Wasp assigns generated values to those fields by default (e.g. `username: nice-blue-horse-14357` and a strong random `password`). This is a historical coupling between auth methods that will be removed over time. If you'd like to change this behavior, these values can be overridden as described below.

:::tip Overriding Defaults
It is also posslbe to [override the default](features#overrides-for-social-login-providers) login behaviors that Wasp provides for you. This allows you to create custom setups, such as allowing Users to define a username rather than the default random username assigned by Wasp on initial Login.
:::

#### `externalAuthEntity`
Anytime an authentication method is used that relies on an external authorization provider, for example, Google, we require an `externalAuthEntity` specified in `auth`, in addition to the `userEntity`, that contains the following configuration:

```wasp {4,14}
//...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
//...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    //...
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```
:::note
the same `externalAuthEntity` can be used across different social login providers (e.g., both GitHub and Google can use the same entity).
:::
#### UI helpers

Wasp provides sign-in buttons, logos and URLs for your login page:

```jsx
...
import { SignInButton as GoogleSignInButton, signInUrl as googleSignInUrl, logoUrl as googleLogoUrl } from '@wasp/auth/helpers/Google'
import { SignInButton as GitHubSignInButton, signInUrl as gitHubSignInUrl, logoUrl as gitHubLogoUrl } from '@wasp/auth/helpers/GitHub'

const Login = () => {
  return (
    <>
      ...

      <GoogleSignInButton/>
      <GitHubSignInButton/>
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
      <a href={gitHubSignInUrl}>Sign in with GitHub</a>
    </>
  )
}

export default Login
```

If you need more customization than what the buttons provide, you can create your own custom components using the `signInUrl`s.

#### Overrides

When a user signs in for the first time, Wasp will create a new User account and link it to the chosen Auth Provider account for future logins. If the `userEntity` contains a `username` field it will default to a random dictionary phrase that does not exist in the database, such as `nice-blue-horse-27160`. This is a historical coupling between auth methods that will be removed over time.

If you would like to allow the user to select their own username, or some other sign up flow, you could add a boolean property to your `User` entity indicating the account setup is incomplete. You can then check this user's property on the client with the [`useAuth()`](#useauth) hook and redirect them when appropriate
  - e.g. check on homepage if `user.isAuthSetup === false`, redirect them to `EditUserDetailsPage` where they can edit the `username` property.

Alternatively, you could add a `displayName` property to your User entity and assign it using the details of their provider account. Below is an example of how to do this by using:
  - the `getUserFieldsFn` function to configure the user's `username` or `displayName` from their provider account

We also show you how to customize the configuration of the Provider's settings using:
  - the `configFn` function

```wasp title=main.wasp {9,10,13,14,26}
app Example {
  //...

  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      google: {
        configFn: import { config } from "@server/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
      },
      gitHub: {
        configFn: import { config } from "@server/auth/github.js",
        getUserFieldsFn: import { getUserFields } from "@server/auth/github.js"
      }
    },

   //...
  }
}

entity User {=psl
    id          Int     @id @default(autoincrement())
    username    String  @unique
    password    String
    displayName String?
    externalAuthAssociations  SocialLogin[]
psl=}

//...

```


#### `configFn`

This function should return an object with the following shape:
<Tabs>
<TabItem value="google" label="Google" default>

```js title=src/server/auth/google.js
export function config() {
  // ...
  return {
    clientID, // look up from env or elsewhere,
    clientSecret, // look up from env or elsewhere,
    scope: ['profile'] // must include at least 'profile' for Google
  }
}

// ...
```

<p>Here is a link to the <a href="https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/server/src/routes/auth/passport/google/defaults.js">default implementations</a> as a reference</p>
</TabItem>
<TabItem value="github" label="GitHub">

```js title=src/server/auth/github.js
export function config() {
  // ...
  return {
    clientID, // look up from env or elsewhere,
    clientSecret, // look up from env or elsewhere,
    scope: [] // default is an empty array for GitHub
  }
}

// ...
```

<p>Here is a link to the <a href="https://github.com/wasp-lang/wasp/blob/release/waspc/data/Generator/templates/server/src/routes/auth/passport/github/defaults.js">default implementations</a> as a reference</p>
</TabItem>
</Tabs>

#### `getUserFieldsFn`

This function should return the user fields to use when creating a new user upon their first time logging in with a Social Auth Provider. The context contains a User entity for DB access, and the args are what the OAuth provider responds with. Here is how you could generate a username based on the Google display name. In your model, you could choose to add more attributes and set additional information.
  ```js title=src/server/auth/google.js
  import { generateAvailableUsername } from '@wasp/core/auth.js'

  // ...

  export async function getUserFields(_context, args) {
    const username = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
    return { username }
  }
  ```

  Or you could set the optional `displayName` property on the `User` entity instead:
  ```js title=src/server/auth/google.js
  import { generateAvailableDictionaryUsername, generateAvailableUsername } from '@wasp/core/auth.js'

  // ...

  export async function getUserFields(_context, args) {
    const username = await generateAvailableDictionaryUsername()
    const displayName = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
    return { username, displayName }
  }
  ```
  - `generateAvailableUsername` takes an array of Strings and an optional separator and generates a string ending with a random number that is not yet in the database. For example, the above could produce something like "Jim.Smith.3984" for a Google user Jim Smith.
  - `generateAvailableDictionaryUsername` generates a random dictionary phrase that is not yet in the database. For example, `nice-blue-horse-27160`.


### Validation Error Handling
When creating, updating, or deleting entities, you may wish to handle validation errors. We have exposed a class called `AuthError` for this purpose. This could also be combined with [Prisma Error Helpers](/docs/language/features#prisma-error-helpers).

#### `import statement`:
```js
import AuthError from '@wasp/core/AuthError.js'
```

##### Example of usage:
```js
try {
  await context.entities.User.update(...)
} catch (e) {
  if (e instanceof AuthError) {
    throw new HttpError(422, 'Validation failed', { message: e.message })
  } else {
    throw e
  }
}
```

## Accessing the currently logged in user
When authentication is enabled in a Wasp app, we need a way to tell whether a user is logged in and access its data.
With that, we can further implement access control and decide which content is private and which public.

#### On the client
On the client, Wasp provides a React hook you can use in functional components - `useAuth`.
This hook is actually a thin wrapper over Wasp's [`useQuery` hook](http://localhost:3002/docs/language/features#the-usequery-hook) and returns data in the same format.

### `useAuth()`
#### `import statement`:
```js
import useAuth from '@wasp/auth/useAuth'
```

##### Example of usage:
```js title="src/client/pages/MainPage.js"
import React from 'react'

import { Link } from 'react-router-dom'
import useAuth from '@wasp/auth/useAuth'
import logout from '@wasp/auth/logout.js'
import Todo from '../Todo.js'
import '../Main.css'

const Main = () => {
  const { data: user } = useAuth()

  if (!user) {
    return (
      <span>
        Please <Link to='/login'>login</Link> or <Link to='/signup'>sign up</Link>.
      </span>
    )
  } else {
    return (
      <>
        <button onClick={logout}>Logout</button>
        <Todo />
      < />
    )
  }
}

export default Main
```

#### On the server

### `context.user`

When authentication is enabled, all operations (actions and queries) will have access to the `user` through the `context` argument. `context.user` will contain all the fields from the user entity except for the password.

##### Example of usage:
```js title="src/server/actions.js"
import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (task, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  return Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id }
      }
    }
  })
}
```
In order to implement access control, each operation is responsible for checking `context.user` and
acting accordingly - e.g. if `context.user` is `undefined` and the operation is private then user
should be denied access to it.

## Client configuration

You can configure the client using the `client` field inside the `app`
declaration,

```wasp
app MyApp {
  title: "My app",
  // ...
  client: {
    rootComponent: import Root from "@client/Root.jsx",
    setupFn: import mySetupFunction from "@client/myClientSetupCode.js"
  }
}
```

`app.client` is a dictionary with the following fields:

#### `rootComponent: ClientImport` (optional)

`rootComponent` defines the root component of your client application. It is
expected to be a React component, and Wasp will use it to wrap your entire app.
It must render its children, which are the actual pages of your application.

You can use it to define a common layout for your application:

```jsx title="src/client/Root.jsx"
export default async function Root({ children }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```

You can use it to set up various providers that your application needs:

```jsx title="src/client/Root.jsx"
import store from './store'
import { Provider } from 'react-redux'

export default async function Root({ children }) {
  return (
    <Provider store={store}>
      {children}
    </Provider>
  )
}
```

As long as you render the children, you can do whatever you want in your root
component. Here's an example of a root component both sets up a provider and
renders a custom layout:

```jsx title="src/client/Root.jsx"
import store from './store'
import { Provider } from 'react-redux'

export default function Root({ children }) {
  return (
    <Provider store={store}>
      <Layout>
        {children}
      </Layout>
    </Provider>
  )
}

function Layout({ children }) {
  return (
    <div>
      <header>
        <h1>My App</h1>
      </header>
      {children}
      <footer>
        <p>My App footer</p>
      </footer>
    </div>
  )
}
```


#### `setupFn: ClientImport` (optional)

`setupFn` declares a JavaScript function that Wasp executes on the client
before everything else. It is expected to be asynchronous, and
Wasp will await its completion before rendering the page. The function takes no
arguments, and its return value is ignored.

You can use this function to perform any custom setup (e.g., setting up
client-side periodic jobs).

Here's a dummy example of such a function:

```js title="src/client/myClientSetupCode.js"
export default async function mySetupFunction() {
  let count = 1;
  setInterval(
    () => console.log(`You have been online for ${count++} hours.`),
    1000 * 60 * 60,
  )
}
```

##### Overriding default behaviour for Queries
As mentioned, our `useQuery` hook uses _react-query_'s hook of the same name.
Since _react-query_ comes configured with aggressive but sane default options,
you most likely won't have to change those defaults for all Queries (you can
change them for a single Query using the `options` object, as described
[here](#the-usequery-hook)).

Still, if you do need the global defaults, you can do so inside client setup
function. Wasp exposes a `configureQueryClient` hook that lets you configure
_react-query_'s `QueryClient` object:


```js title="src/client/myClientSetupCode.js"
import { configureQueryClient } from '@wasp/queryClient'

export default async function mySetupFunction() {
  // ... some setup
  configureQueryClient({
    defaultOptions: {
      queries: {
        staleTime: Infinity,
      }
    }
  })
  // ... some more setup
}
```

Make sure to pass in an object expected by the `QueryClient`'s constructor, as
explained in
[_react-query_'s docs](https://tanstack.com/query/v4/docs/react/reference/QueryClient).

## Server configuration

Via `server` field of `app` declaration, you can configure behaviour of the Node.js server (one that is executing wasp operations).

```wasp
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import mySetupFunction from "@server/myServerSetupCode.js"
  }
}
```

`app.server` is a dictionary with following fields:

#### `middlewareConfigFn: ServerImport` (optional)

The import statement to an Express middleware config function. This is a global modification affecting all operations and APIs. See [the guide here](/docs/guides/middleware-customization#1-customize-global-middleware).

#### `setupFn: ServerImport` (optional)

`setupFn` declares a JS function that will be executed on server start. This function is expected to be async and will be awaited before the server starts accepting any requests.

It gives you an opportunity to do any custom setup, e.g. setting up additional database/websockets or starting cron/scheduled jobs.

The `setupFn` function receives the `express.Application` and the `http.Server` instances as part of its context. They can be useful for setting up any custom server routes or for example, setting up `socket.io`.
```ts
export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>

export type ServerSetupFnContext = {
  app: Application, // === express.Application
  server: Server,   // === http.Server
}
```

As an example, adding a custom route would look something like:
```ts title="src/server/myServerSetupCode.ts"
import { ServerSetupFn, Application } from '@wasp/types'

const mySetupFunction: ServerSetupFn = async ({ app }) => {
  addCustomRoute(app)
}

function addCustomRoute(app: Application) {
  app.get('/customRoute', (_req, res) => {
    res.send('I am a custom route')
  })
}
```

In case you want to store some values for later use, or to be accessed by the Operations, recommended way is to store those in variables in the same module/file where you defined the javascript setup function and then expose additional functions for reading those values, which you can then import directly from Operations and use. This effectively turns your module into a singleton whose construction is performed on server start.

Dummy example of such function and its usage:

```js title="src/server/myServerSetupCode.js"
let someResource = undefined

const mySetupFunction = async () => {
  // Let's pretend functions setUpSomeResource and startSomeCronJob
  // are implemented below or imported from another file.
  someResource = await setUpSomeResource()
  startSomeCronJob()
}

export const getSomeResource = () => someResource

export default mySetupFunction
```

```js title="src/server/queries.js"
import { getSomeResource } from './myServerSetupCode.js'

...

export const someQuery = async (args, context) => {
  const someResource = getSomeResource()
  return queryDataFromSomeResource(args, someResource)
}
```


## .env

Your project will likely be using environment variables for configuration, typically to define connection to the database, API keys for external services and similar.

When in production, you will typically define environment variables through mechanisms provided by your hosting provider.

However, when in development, you might also need to supply certain environment variables, and to avoid doing it "manually", Wasp supports `.env` (dotenv) files where you can define environment variables that will be used during development (they will not be used during production).

Since environmental variables are usually different for server-side and client apps, in Wasp root directly you can create two files, `.env.server` for server-side of your Wasp project, and `.env.client` for client side (or web app) of Wasp project.


`.env.server` and `.env.client` files have to be defined in the root of your Wasp project.

`.env.server` and `.env.client` files should not be commited to the version control - we already ignore it by default in the .gitignore file we generate when you create a new Wasp project via `wasp new` cli command.

Variables are defined in `.env.server` or `.env.client` files in the form of `NAME=VALUE`, for example:
```
DATABASE_URL=postgresql://localhost:5432
MY_VAR=somevalue
```

Any env vars defined in the `.env.server` / `.env.client` files will be forwarded to the server-side / web app of your Wasp project and therefore accessible in your javascript code via `process.env`, for example:
```js
console.log(process.env.DATABASE_URL)
```

## Database

Via `db` field of `app` declaration, you can configure the database used by Wasp.

```wasp
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL,
    seeds: [
      import devSeed from "@server/dbSeeds.js"
    ]
  }
}
```

`app.db` is a dictionary with following fields:

#### - `system: DbSystem` (Optional)
Database system that Wasp will use. It can be either `PostgreSQL` or `SQLite`.
If not defined, or even if whole `db` field is not present, default value is `SQLite`.
If you add/remove/modify `db` field, run `wasp db migrate-dev` to apply the changes.

#### - `seeds: [ServerImport]` (Optional)
Defines seed functions that you can use via `wasp db seed` to seed your database with initial data.
Check out [Seeding](#seeding) section for more details.

### SQLite
Default database is `SQLite`, since it is great for getting started with a new project (needs no configuring), but it can be used only in development - once you want to deploy Wasp to production you will need to switch to `PostgreSQL` and stick with it.
Check below for more details on how to migrate from SQLite to PostgreSQL.

### PostgreSQL
When using `PostgreSQL` as your database (`app: { db: { system: PostgreSQL } }`), you will need to make sure you have a postgres database running during development (when running `wasp start` or doing `wasp db ...` commands).

### Using Wasp provided dev database

Wasp provides `wasp start db` command that starts the default dev db for you.

Your Wasp app will automatically connect to it once you have it running via `wasp start db`, no additional configuration is needed. This command relies on Docker being installed on your machine.

### Connecting to existing database

If instead of using `wasp start db` you would rather spin up your own dev database or connect to some external database, you will need to provide Wasp with `DATABASE_URL` environment variable that Wasp will use to connect to it.

The easiest way to provide the needed `DATABASE_URL` environment variable is by adding it to the [.env.server](https://wasp-lang.dev/docs/language/features#env) file in the root dir of your Wasp project (if that file doesn't yet exist, create it).

You can also set it per command by doing `DATABASE_URL=<my-db-url> wasp ...` -> this can be useful if you want to run specific `wasp` command on a specific database.
Example: you could do `DATABASE_URL=<my-db-url> wasp db seed myProdSeed` to seed data for a fresh staging or production database.

### Migrating from SQLite to PostgreSQL
To run Wasp app in production, you will need to switch from `SQLite` to `PostgreSQL`.

1. Set `app.db.system` to `PostgreSQL`.
3. Delete old migrations, since they are SQLite migrations and can't be used with PostgreSQL: `rm -r migrations/`.
3. Run `wasp start db` to start your new db running (or check instructions above if you prefer using your own db). Leave it running, since we need it for the next step.
4. In a different terminal, run `wasp db migrate-dev` to apply new changes and create new, initial migration.
5. That is it, you are all done!

### Seeding

**Database seeding** is a term for populating database with some initial data.

Seeding is most commonly used for two following scenarios:
 1. To put development database into a state convenient for testing / playing with it.
 2. To initialize dev/staging/prod database with some essential data needed for it to be useful,
    for example default currencies in a Currency table.

#### Writing a seed function

Wasp enables you to define multiple **seed functions** via `app.db.seeds`:

```wasp
app MyApp {
  // ...
  db: {
    // ...
    seeds: [
      import { devSeedSimple } from "@server/dbSeeds.js",
      import { prodSeed } from "@server/dbSeeds.js"
    ]
  }
}
```

Each seed function is expected to be an async function that takes one argument, `prismaClient`, which is a [Prisma Client](https://www.prisma.io/docs/concepts/components/prisma-client/crud) instance that you can use to interact with the database.
This is the same instance of Prisma Client that Wasp uses internally, so you e.g. get password hashing for free.

Since a seed function is part of the server-side code, it can also import other server-side code, so you can and will normally want to import and use Actions to perform the seeding.

Example of a seed function that imports an Action (+ a helper function to create a user):

```js
import { createTask } from './actions.js'

export const devSeedSimple = async (prismaClient) => {
  const user = await createUser(prismaClient, {
      username: "RiuTheDog",
      password: "bark1234"
  })

  await createTask(
    { description: "Chase the cat" },
    { user, entities: { Task: prismaClient.task } }
  )
}

async function createUser (prismaClient, data) {
  const { password, ...newUser } = await prismaClient.user.create({ data })
  return newUser
}
```

#### Running seed functions

 - `wasp db seed`: If you have just one seed function, it will run it. If you have multiple, it will interactively ask you to choose one to run.

 - `wasp db seed <seed-name>`: It will run the seed function with the specified name, where the name is the identifier you used in its `import` expression in the `app.db.seeds` list. Example: `wasp db seed devSeedSimple`.

:::tip
  Often you will want to call `wasp db seed` right after you ran `wasp db reset`: first you empty your database, then you fill it with some initial data.
:::


## Email sender

#### `provider: EmailProvider` (required)

We support multiple different providers for sending e-mails: `SMTP`, `SendGrid` and `Mailgun`.

### SMTP

SMTP e-mail sender uses your SMTP server to send e-mails.

Read [our guide](/docs/guides/sending-emails#using-the-smtp-provider) for setting up SMTP for more details.


### SendGrid

SendGrid is a popular service for sending e-mails that provides both API and SMTP methods of sending e-mails. We use their official SDK for sending e-mails.

Check out [our guide](/docs/guides/sending-emails#using-the-sendgrid-provider) for setting up Sendgrid for more details.

### Mailgun

Mailgun is a popular service for sending e-mails that provides both API and SMTP methods of sending e-mails. We use their official SDK for sending e-mails.

Check out [our guide](/docs/guides/sending-emails#using-the-mailgun-provider) for setting up Mailgun for more details.

#### `defaultSender: EmailFromField` (optional)

You can optionally provide a default sender info that will be used when you don't provide it explicitly when sending an e-mail.

```wasp
app MyApp {
  title: "My app",
  // ...
  emailSender: {
    provider: SMTP,
    defaultFrom: {
      name: "Hello",
      email: "hello@itsme.com"
    },
  },
}
```

After you set up the email sender, you can use it in your code to send e-mails. For example, you can send an e-mail when a user signs up, or when a user resets their password.

### Sending e-mails

<SendingEmailsInDevelopment />

To send an e-mail, you can use the `emailSender` that is provided by the `@wasp/email` module.

```ts title="src/actions/sendEmail.js"
import { emailSender } from '@wasp/email/index.js'

// In some action handler...
const info = await emailSender.send({
    to: 'user@domain.com',
    subject: 'Saying hello',
    text: 'Hello world',
    html: 'Hello <strong>world</strong>'
})
```
