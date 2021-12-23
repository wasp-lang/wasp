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

### Using entities

Entity-system in Wasp is based on [Prisma](http://www.prisma.io), and currently Wasp provides only a thin layer
on top of it. The workflow is as follows:

1. Wasp developer creates/updates some of the entities in `.wasp` file.
2. Wasp developer runs `wasp db migrate-dev`.
3. Migration data is generated in `migrations/` folder (and should be commited).
4. Wasp developer uses Prisma JS API to work with the database when in Operations.

Currently entities can be accessed only in Operations (Queries & Actions), so check their part of docs for more info on how to use entities in their context.

## Queries and Actions (aka Operations)

In Wasp, main interaction between client and server happens via Operations, of which two types exist: Queries and Actions.

### Query

Queries are NodeJS functions that don't modify any state. Normally they fetch certain resources, process them and return result. They are executed on server. 

To create a Wasp Query, we need two things: declaration in Wasp and implementation in NodeJS:

1. `query` declaration in Wasp:
```c title="main.wasp"
// ...
query getTasks {
  fn: import { getAllTasks } from "@ext/foo.js"
}
```
`query` declaration type has two fields:
- `fn: ExtImport` (required)
- `entities: [Entity]` (optional)

2. Implemenation in NodeJS:
```js title="ext/foo.js"
// ...
export getAllTasks = async (args, context) => {
  return [
    { description: "Buy some eggs", isDone: true },
    { description: "Make an omelette", isDone: false }
  ]
}
```

NodeJS function above has to be async and will be passed query arguments as first argument and additional context as second argument.

By declaring a NodeJS function as a Wasp query, following happens:
- Wasp generates HTTP API route on the NodeJS server that calls the NodeJS query function.
- Wasp generates JS function on the client that has the name under which query was declared and takes same arguments as the NodeJS query function. Internally it uses above mentioned HTTP API route to call the NodeJS query function.

On client, you can import generated query JS function as `import getTasks from '@wasp/queries/getTasks.js'`.
Then, you can either use it directly, or you can use it via special `useQuery` React hook (provided by Wasp**) to make it reactive.

On server, you can import it the same way as on client, and then you can call it directly.

**NOTE**: Wasp will not stop you from importing NodeJS function directly on server, e.g. `import { getAllTasks } from "./foo.js"`, but you shouldn't do it, because it will import pure NodeJS function and not a query recognized by Wasp, so it will not get all the features of a Wasp query.

#### useQuery
`useQuery` hook provided by Wasp is actually just a thin wrapper for `useQuery` hook from [react-query](https://github.com/tannerlinsley/react-query).

You can import it as `import { useQuery } from '@wasp/queries'`.

Wasp `useQuery` takes three args:
- `queryFn`: client query function generated by Wasp based on query declaration, e.g. one you get by importing in JS like this: `import getTasks from '@wasp/queries/getTasks.js'`.
- `queryFnArgs`
- `config`: react-query `config`.

It behaves exactly the same as [useQuery from react-query](https://react-query.tanstack.com/docs/api#usequery), only it doesn't take the key, that is handled automatically instead.

Example of usage:
```js
import React from 'react'
import { useQuery } from '@wasp/queries'
import getTasks from '@wasp/queries/getTasks'

const MyComponent = (props) => {
  const { data: tasks, error } = useQuery(getTasks)
  return <div> { JSON.stringify(tasks || error) } </div>
}
```

#### Error handling
For security reasons, all errors thrown in the query NodeJS function are sent to the client via HTTP API as 500 errors, with any further details removed, so that any unpredicted errors don't make it out with possibly sensitive data.

If you do want to throw an error that will pass some information to the client, you can use `HttpError` in your NodeJS query function:
```js
import HttpError from '@wasp/core/HttpError.js'

export getTasks = async (args, context) => {
  const statusCode = 403
  const message = 'You can\'t do this!'.
  const data = { foo: 'bar' }
  throw new HttpError(statusCode, message, data)
}
```

and then in client it will be thrown as an Error with corresponding `.message` and `.data` fields (if status code is 4xx - otherwise `message` and `data` will not be forwarded to the client, for security reasons).

This ensures that no error will accidentally leak out from the server, potentionally exposing sensitive data or implementation details.

#### Using entities
Most often, resources used by Operations will be Entities.

To use an Entity in your Operation, declare in Wasp that Operation uses it:
```c {4} title="todoApp.wasp"
// ...
query getTasks {
  fn: import { getAllTasks } from "@ext/foo.js",
  entities: [Task]
}
```

This will inject specified entity into the context of your Operation.
Now, you can access Prisma API for that entity like this:
```js title="ext/foo.js"
// ...
export getAllTasks = async (args, context) => {
  return context.entities.Task.findMany({})
}
```

where `context.entities.Task` actually exposes `prisma.task` from [Prisma API](https://www.prisma.io/docs/reference/tools-and-interfaces/prisma-client/crud).

#### Cache invalidation
One of the trickiest part of managing web app state is making sure that data which queries are showing is up to date.

Since Wasp is using react-query for managing queries, that means we want to make sure that parts of react-query cache are invalidated when we know they are not up to date any more.

This can be done manually, by using mechanisms provided by react-query (refetch, direct invalidation).
However, that can often be tricky and error-prone, so Wasp offers quick and effective solution to get you started: automatic invalidation of query cache based on entities that queries / actions are using.

Specifically, if Action A1 uses Entity E1 and Query Q1 also uses Entity E1 and Action A1 is executed, Wasp will recognize that Q1 might not be up-to-date any more and will therefore invalidate its cache, making sure it gets updated.

In practice, this means that without really even thinking about it, Wasp will make sure to keep the queries up to date for you in regard with the changes done by actions.

On the other hand, this kind of automatic invalidation of cache can be wasteful (updating when not needed) and will not work if other resources than entities are used. In that case, make sure to use mechanisms provided by react-query for now, and expect more direct support in Wasp for handling those use cases in a nice, elegant way.


### Action

Actions are very similar to Queries, so similar that we will only list the differences:
1. They can modify state (queries can't).
2. There is no special React hook for them (like `useQuery` for Queries), you just call them directly.
3. They are declared in Wasp in same way as Queries, but keyword is `action`, not `query`.

More differences and action/query specific features will come in the future versions of Wasp.


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
