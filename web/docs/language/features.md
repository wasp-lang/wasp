---
title: Features
---

## App

There can be only one declaration of `app` type per Wasp project.
It serves as a starting point and defines global properties of your app.

```css
app todoApp {
  title: "ToDo App",
  head: [  // optional
    "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Roboto:300,400,500&display=swap\" />"
  ]
}
```

### Fields

#### `title: string` (required)
Title of your app. It will be displayed in the browser tab, next to the favicon.

#### `head: [string]` (optional)
Head of your HTML Document. Your app's metadata (styles, links, etc) can be added here.

#### `auth: dict` (optional)
Authentication and authorization configuration.
Check [`app.auth`](/docs/language/features#authentication--authorization) for more details.

#### `db: dict` (optional)
Database configuration.
Check [`app.db`](/docs/language/features#database) for more details.

#### `server: dict` (optional)
Server configuration.
Check [`app.server`](/docs/language/features#server) for more details.

#### `dependencies: [(string, string)]` (optional)
List of dependencies (external libraries).
Check [`app.dependencies`](/docs/language/features#dependencies) for more details.

## Page

`page` declaration is the top-level layout abstraction. Your app can have multiple pages.

```css
page MainPage {
  component: import Main from "@ext/pages/Main",
  authRequired: false  // optional
}
```

Normally you will also want to associate `page` with a `route`, otherwise it won't be accessible in the app.

### Fields

#### `component: ExtImport` (required)
Import statement of the React element that implements the page component.
See importing external code for details.

#### `authRequired: bool` (optional)
Can be specified only if [`app.auth`](/docs/language/features#authentication--authorization) is defined.

If set to `true`, only authenticated users will be able to access this page. Unauthenticated users will be redirected to a route defined by `onAuthFailedRedirectTo` property within `app.auth`.

If `authRequired` is set to `true`, the React component of a page (specified by `component` property) will be provided `user` object as a prop.

Check out this [section of our Todo app tutorial](/docs/tutorials/todo-app/auth#updating-main-page-to-check-if-user-is-authenticated) for an example of usage.

## Route

`route` declaration provides top-level routing functionality in Wasp.

```css
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
```css
route TaskRoute { path: "/task/:id", to: TaskPage }
```
For details on URL path format check [React Router](https://reactrouter.com/web/)
documentation.

### Accessing route parameters in a page component

Since Wasp under the hood generates code with [React Router](https://reactrouter.com/web/),
the same rules apply when accessing URL params in your React components. Here is an example just to get you
started:

```c title="todoApp.wasp"
// ...
route TaskRoute { path: "/task/:id", to: TaskPage }
page TaskPage {
  component: import Task from "@ext/pages/Task"
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

```c title="todoApp.wasp"
// ...
route HomeRoute { path: "/home", to: HomePage }
page HomePage {
  component: import Home from "@ext/pages/Home"
}
```

```jsx title="pages/OtherPage.js"
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

```css
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

Currently entities can be accessed only in Operations (Queries & Actions), so check their part of docs for more info on how to use entities in their context.

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
A Query must be implemented as an `async` NodeJS function that takes two arguments.
Since both arguments are positional, you can name the parameters however you want, but we'll stick with `args` and `context`:
1. `args`:  An object containing all the arguments (i.e., payload) **passed to the Query by the caller** (e.g., filtering conditions).
Take a look at [the examples of usage](#using-the-query) to see how to pass this object to the Query.
3. `context`: An additional context object **injected into the Query by Wasp**. This object contains user session information, as well as information about entities. The examples here won't use the context for simplicity purposes. You can read more about it in the [section about using entities in queries](#using-entities-in-queries).

Here's an example of two simple Queries:
```js title="ext/queries.js"
// our "database"
const tasks = [
  { id: 1, description: "Buy some eggs", isDone: true },
  { id: 2, description: "Make an omelette", isDone: false },
  { id: 3, description: "Eat breakfast", isDone: false }
]


// You don't need to use the arguments if you don't need them
export const getAllTasks = async () => {
  return tasks;
}

// The 'args' object is something sent by the caller (most often from the client)
export const getFilteredTasks = async (args) => {
  const { isDone } = args;
  return tasks.filter(task => task.isDone === isDone)
}
```

#### Declaring a Query in Wasp
After implementing your Queries in NodeJS, all that's left to do before using them is tell Wasp about it!
You can easily do this with the `query` declaration, which supports the following fields:
- `fn: ExtImport` (required) - The import statement of the Query's NodeJs implementation.
- `entities: [Entity]` (optional) - A list of entities you wish to use inside your Query.
We'll leave this option aside for now. You can read more about it [here](#using-entities-in-queries).

Wasp Queries and their implementations don't need to (but can) have the same name, so we will keep the names different to avoid confusion.
With that in mind, this is how you might declare the Queries that use the implementations from the previous step:
```c title="main.wasp"
// ...

// Again, it most likely makes sense to name the Wasp Query after
// its implementation. We're changing the name to emphasize the difference.

query fetchAllTasks {
  fn: import { getAllTasks } from "@ext/queries.js"
}

query fetchFilteredTasks {
  fn: import { getFilteredTasks } from "@ext/queries.js"
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
- `queryFnArgs` (optional): The arguments object (payload) you wish to pass into the query. The query's NodeJS implementation will receive this object as its first positional argument.
- `config` (optional): A _react-query_ `config` object.

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
        <p>All tasks: { JSON.stringify(allTasks || error1) }</p>
        <p>Finished tasks: { JSON.stringify(doneTasks || error2) }</p>
    </div>
  )
}

export default MainPage
```

#### Error Handling
For security reasons, all exceptions thrown in the Query's NodeJS implementation are sent to the client as responses with the HTTP status code `500`, with all other details removed.
Hiding error details by default helps against accidentally leaking possibly sensitive information over the network.

If you do want to pass additional error information to the client, you can construct and throw an appropriate `HttpError` in your NodeJS Query function:
```js title=ext/queries.js
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

```c {4,9} title="main.wasp"

query fetchAllTasks {
  fn: import { getAllTasks } from "@ext/queries.js",
  entities: [Task]
}

query fetchFilteredTasks {
  fn: import { getFilteredTasks } from "@ext/queries.js",
  entities: [Task]
}
```

Wasp will inject the specified Entity into the Query's `context` argument, giving you access to the Entity's Prisma API:
```js title="ext/queries.js"
// ...

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
2. Since Actions don't need to be reactive, Wasp doesn't provide a React hook for them (like `useQuery` for Queries) - you just call them directly.
3. `action` declarations in Wasp are mostly identical to `query` declarations. The only difference is in the declaration's name.

Here's an implementation of a simple Action:

```js title=actions.js
export const sayHi = async () => {
  console.log('The client said Hi!')
}
```
Its corresponding declaration in Wasp:

```c title="main.wasp"
// ...

action sayHi {
  fn: import { sayHi } from "@ext/actions.js"
}
```
And an example of how to import and call the declared Action:

```js
import sayHi from '@wasp/actions/sayHi'

// ...

sayHi()
```


More differences and Action/Query specific features will come in future versions of Wasp.

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

## Job Queues

If you have long running tasks that require reliable, retriable, and/or schedulable (possibly recurring) work to be performed that should not be part of the normal request-response cycle, you may need a `job`! Wasp allows you to register `job` handler functions to be executed by a job executor, whereby you can manually enqueue work from JavaScript on the server, or have it automatically invoked for you via a cron schedule.

### Job Executors

Job executors hande the scheduling and execution of our job functions. They are the primary abstraction we provide that allows you to write one async JavaScript `job` handler function, but have it executed in different job queue contexts by changing a single line in your `.wasp` file.

#### pg-boss

We have selected [pg-boss](https://github.com/timgit/pg-boss/) as our first job executor to handle the low-volume, basic job queue workloads many web applications have (think sending emails, connecting to external web services, etc.). By using PostgreSQL as it's storage and synchronization mechanism, it allows us to provide many job queue pros without any additional infrastructure or complex management.

_Note: All work is performed alongside the web server's Node event loop, making it unsuitable for CPU-intensive tasks. Additionally, we do not support independent, horizontal scaling of workers. We plan to handle both of these concerns with future job executors. Please let us know on Discord what you'd like to see. :D_

### Basics

To register a `job` in Wasp, you simply add a declaration like the following:

```css title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@ext/jobs/bar.js",
    options: {=json { "retryLimit": 1 } json=} // optional
  }
}
```

Then, in your [Operations](/docs/language/features#queries-and-actions-aka-operations) or [setupFn](/docs/language/features#setupfn-extimport-optional), you can enqueue work to be done:
```js
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

const submittedJob = await mySpecialJob.submit({ job: "args" })
console.log(await submittedJob.pgBoss.details())

// Or, if you'd prefer it to execute in the future, just add a .delay().
// It takes a number of seconds, Date, or ISO date string.
await mySpecialJob.delay(10).submit({ job: "args" })
```

And that is it! Your job will be executed by the job executor as if you called `foo({ job: "args" })`.

### Scheduled Jobs

If you have work that needs to be done on some recurring basis, you can add a `schedule` to your job declaration:

```css  {6-10} title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@ext/jobs/bar.js"
  },
  schedule: {
    cron: "0 * * * *",
    performFnArg: {=json { "foo": "bar" } json=}, // optional
    options: {=json { "retryLimit": 2 } json=} // optional
  }
}
```

In this example, you do _not_ need to invoke anything in JavaScript. You can imagine `foo({ "foo": "bar" })` getting automatically scheduled and invoked for you every hour.

### Syntax

#### Wasp files

#### JavaScript API

## Dependencies

You can specify additional npm dependencies via `dependencies` field in `app` declaration, in following way:

```c
app MyApp {
  title: "My app",
  // ...
  dependencies: [
    ("redux", "^4.0.5"),
    ("react-redux", "^7.1.3")
  ]
)
```

You will need to re-run `wasp start` after adding a dependency for Wasp to pick it up.

**NOTE**: In current implementation of Wasp, if Wasp is already internally using certain npm dependency with certain version specified, you are not allowed to define that same npm dependency yourself while specifying different version.
If you do that, you will get an error message telling you which exact version you have to use for that dependency.
This means Wasp dictates exact versions of certain packages, so for example you can't choose version of React you want to use.
In the future, we will add support for picking any version you like, but we have not implemented that yet. Check [issue #59](https://github.com/wasp-lang/wasp/issues/59) to check out the progress or contribute.


## Authentication & Authorization

Wasp provides authentication and authorization support out-of-the-box. Enabling it for your app is optional and can be done by configuring `auth` field of the `app` declaration:

```css
app MyApp {
  title: "My app",
  // ...
  auth: {
    userEntity: User,
    methods: [ EmailAndPassword ],
    onAuthFailedRedirectTo: "/someRoute"
  }
}
```

`app.auth` is a dictionary with following fields:

#### `userEntity: entity` (required)
Entity which represents the user (sometimes also referred to as *Principal*).

#### `methods: [AuthMethod]` (required)
List of authentication methods that Wasp app supports. Currently supported methods are:
* `EmailAndPassword`: Provides support for authentication with email address and a password.

#### `onAuthFailedRedirectTo: String` (required)
Path where an unauthenticated user will be redirected to if they try to access a private page (which is declared by setting `authRequired: true` for a specific page).
Check out this [section of our Todo app tutorial](/docs/tutorials/todo-app/auth#updating-main-page-to-check-if-user-is-authenticated) to see an example of usage.

#### `onAuthSucceededRedirectTo: String` (optional)
Path where a successfully authenticated user will be sent upon successful login/signup.
Default value is "/".

### Email and Password

`EmailAndPassword` authentication method makes it possible to signup/login into the app by using email address and a password.
This method requires that `userEntity` specified in `auth` contains `email: string` and `password: string` fields.

We provide basic validations out of the box, which you can customize as shown below. Default validations are:
- `email`: non-empty
- `password`: non-empty, at least 8 characters, and contains a number

#### High-level API

The quickest way to get started is by using the following API generated by Wasp:
- Signup and Login forms at `@wasp/auth/forms/Signup` and `@wasp/auth/forms/Login` routes
- `logout` function
- `useAuth()` React hook

Check our [Todo app tutorial](/docs/tutorials/todo-app/auth) to see how it works. See below for detailed specification of each of these methods.

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
        data: { email: 'some@email.com', password: 'this will be hashed!' }
    })

    // Your custom code after sign-up.
    // ...
    return newUser
}
```

:::info
You don't need to worry about hashing the password yourself! Even when you are using Prisma's client directly and calling `create()` with a plain-text password, Wasp put middleware in place that takes care of hashing it before storing it to the database. An additional middleware also performs field validation.
:::

##### Customizing user entity validations

To disable/enable default validations, or add your own, you can do:
```js
const newUser = context.entities.User.create({
  data: { email: 'some@email.com', password: 'this will be hashed!' },
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
Validations always run on `create()`, but only when the field mentioned in `validates` is present for `update()`. The validation process stops on the first `validator` to return false. If enabled, default validations run first and validate basic properties of both the `'email'` or `'password'` fields.
:::

#### Specification

### `login()`
An action for logging in the user.
```js
login(email, password)
```
#### `email: string`
Email of the user logging in.

#### `password: string`
Password of the user logging in.

#### `import statement`:
```js
import login from '@wasp/auth/login.js'
```
Login is a regular action and can be used directly from the frontend.


### `signup()`
An action for signing in in the user.
```js
signup(userFields)
```
#### `userFields: object`
Fields of user entity which was declared in `auth`.

#### `import statement`:
```js
import signup from '@wasp/auth/signup.js'
```
Signup is a regular action and can be used directly from the frontend.


### `logout()`
An action for logging out the user.
```js
logout()
```

#### `import statement`:
```js
import logout from '@wasp/auth/logout.js'
```

##### Example of usage:
```js
import logout from '@wasp/auth/logout.js'

const SignOut = () => {
  return (
    <button onClick={logout}>Logout</button>
  )
}
```

#### Reset password
Coming soon.

### Updating user's password
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


### Accessing currently logged in user
When authentication is enabled in a Wasp app, we need a way to tell whether a user is logged in and access its data.
With that, we can further implement access control and decide which content is private and which public.

#### On client
On client, Wasp provides `useAuth` React hook to be used within the functional components.
`useAuth` is actually a thin wrapper over Wasp's `useQuery` hook and returns data in the exactly same
format.

### `useAuth()`
#### `import statement`:
```js
import useAuth from '@wasp/auth/useAuth.js'
```

##### Example of usage:
```js title="ext/MainPage.js"
import React from 'react'

import { Link } from 'react-router-dom'
import useAuth from '@wasp/auth/useAuth.js'
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

#### On server

When authentication is enabled, all the operations (actions and queries) will have `user` object
present in the `context` argument. `context.user` will contain all the fields from the user entity
except for the password.

##### Example of usage:
```js title="ext/actions.js"
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

## Server configuration

Via `server` field of `app` declaration, you can configure behaviour of the Node.js server (one that is executing wasp operations).

```c
app MyApp {
  title: "My app",
  // ...
  server: {
    setupFn: import mySetupFunction from "@ext/myServerSetupCode.js"
  }
}
```

`app.server` is a dictionary with following fields:

#### `setupFn: ExtImport` (optional)

`setupFn` declares a JS function that will be executed on server start. This function is expected to be async and will be awaited before server continues with its setup and starts serving any requests.

It gives you an opportunity to do any custom setup, e.g. setting up additional database or starting cron/scheduled jobs.

The javascript function should be async, takes no arguments and its return value is ignored.

In case you want to store some values for later use, or to be accessed by the Operations, recommended way is to store those in variables in the same module/file where you defined the javascript setup function and then expose additional functions for reading those values, which you can then import directly from Operations and use. This effectively turns your module into a singleton whose construction is performed on server start.

Dummy example of such function and its usage:

```js title="ext/myServerSetupCode.js"
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

```js title="ext/queries.js"
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

However, when in development, you might also need to supply certain environment variables, and to avoid doing it "manually", Wasp supports `.env` (dotenv) file where you can define environment variables that will be used during development (they will not be used during production).

`.env` file has to be defined in the root of your Wasp project.

`.env` file should not be commited to the version control - we already ignore it by default in the .gitignore file we generate when you create a new Wasp project via `wasp new` cli command.

Variables are defined in `.env` in the form of `NAME=VALUE`, for example:
```
DATABASE_URL=postgresql://localhost:5432
MY_VAR=somevalue
```

Any env vars defined in the `.env` will be forwarded to the server-side of your Wasp project and therefore accessible in your nodejs code via `process.env`, for example:
```js
console.log(process.env.DATABASE_URL)
```

## Database

Via `db` field of `app` declaration, you can configure the database used by Wasp.

```c
app MyApp {
  title: "My app",
  // ...
  db: {
    system: PostgreSQL
  }
}
```

`app.db` is a dictionary with following fields:

#### `system: DbSystem`
Database system that Wasp will use. It can be either `PostgreSQL` or `SQLite`.
If not defined, or even if whole `db` field is not present, default value is `SQLite`.
If you add/remove/modify `db` field, run `wasp db migrate-dev` to apply the changes.

### SQLite
Default database is `SQLite`, since it is great for getting started with a new project (needs no configuring), but it can be used only in development - once you want to deploy Wasp to production you will need to switch to `PostgreSQL` and stick with it.
Check below for more details on how to migrate from SQLite to PostgreSQL.

### PostgreSQL
When using `PostgreSQL` (`db: { system: PostgreSQL }`), you will need to spin up a postgres database on your own so it runs during development (when running `wasp start` or doing `wasp db ...` commands) and provide Wasp with `DATABASE_URL` environment variable that Wasp will use to connect to it.

One of the easiest ways to do this is by spinning up postgres docker container when you need it with the shell command
```
docker run \
  --rm \
  --publish 5432:5432 \
  -v postgresql-data:/var/lib/postgresql/data \
  --env POSTGRES_PASSWORD=devpass \
  postgres
```
and adding the line
```
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```
to the `.env` file in the root directory of your Wasp project.

### Migrating from SQLite to PostgreSQL
To run Wasp app in production, you will need to switch from `SQLite` to `PostgreSQL`.
1. Set `app.db.system` to `PostgreSQL` and set `DATABASE_URL` env var accordingly (as described [above](/docs/language/features#postgresql)).
2. Delete old migrations, since they are SQLite migrations and can't be used with PostgreSQL: `rm -r migrations/`.
3. Run `wasp db migrate-dev` to apply new changes and create new, initial migration. You will need to have your postgres database running while doing this (check [above](/docs/language/features#postgresql) for easy way to get it running).
