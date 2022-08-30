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

## Jobs

If you have server tasks that you do not want to handle as part of the normal request-response cycle, Wasp allows you to make that function a `job` and it will gain some "superpowers." Jobs will persist between server restarts, can be retried if they fail, and they can even be delayed until the future (or have a recurring schedule)! Some examples where you may want to use a `job` on the server include sending an email, making an HTTP request to some external API, or doing some nightly calculations.

### Job Executors

Job executors handle the scheduling, monitoring, and execution of our jobs.

Wasp allows you to choose which job executor will be used to execute a specific job that you define, which affects some of the finer details of how jobs will behave and how they can be further configured. Each job executor has its pros and cons, which we will explain in more detail below, so you can pick the one that best suits your needs.

Currently, Wasp supports only one type of job executor, which is `PgBoss`, but in the future, it will likely support more.

#### pg-boss

We have selected [pg-boss](https://github.com/timgit/pg-boss/) as our first job executor to handle the low-volume, basic job queue workloads many web applications have. By using PostgreSQL (and [SKIP LOCKED](https://www.2ndquadrant.com/en/blog/what-is-select-skip-locked-for-in-postgresql-9-5/)) as its storage and synchronization mechanism, it allows us to provide many job queue pros without any additional infrastructure or complex management.

Keep in mind that pg-boss jobs run alongside your other server-side code, so they are not appropriate for CPU-heavy workloads. Additionally, some care is required if you modify scheduled jobs. Please see pg-boss details for more information.

<details>
  <summary>pg-boss details</summary>

  pg-boss provides many useful features, which can be found [here](https://github.com/timgit/pg-boss/blob/7.2.1/README.md).

  When you add pg-boss to a Wasp project, it will automatically add a new schema to your database called `pgboss` with some internal tracking tables, including `job` and `schedule`. pg-boss tables have a `name` column in most tables that will correspond to your `job` identifier. Additionally, these tables maintain arguments, states, return values, retry information, start and expiration times, and other metadata required by pg-boss.

  If you need to customize the creation of the pg-boss instance, you can set an environment variable called `PG_BOSS_NEW_OPTIONS` to a stringified JSON object containing [these initialization parameters](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#newoptions). **NOTE**: Setting this overwrites all Wasp defaults, so you must include database connection information as well.

  ##### pg-boss considerations
  - Wasp starts pg-boss alongside your web server's application, where both are simultaneously operational. This means that jobs running via pg-boss and the rest of the server logic (like Operations) share the CPU, therefore you should avoid running CPU-intensive tasks via jobs.
    - Wasp does not (yet) support independent, horizontal scaling of pg-boss-only applications, nor starting them as separate workers/processes/threads.
  - The job name/identifier in your `.wasp` file is the same name that will be used in the `name` column of pg-boss tables. If you change a name that had a `schedule` associated with it, pg-boss will continue scheduling those jobs but they will have no handlers associated, and will thus become stale and expire. To resolve this, you can remove the applicable row from the `schedule` table in the `pgboss` schema of your database.
    - If you remove a `schedule` from a job, you will need to do the above as well.
  - If you wish to deploy to Heroku, you need to set an additional environment variable called `PG_BOSS_NEW_OPTIONS` to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.
- https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js

</details>

### Basic job definition and usage

To declare a `job` in Wasp, simply add a declaration with a reference to an `async` function, like the following:

```css title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@ext/jobs/bar.js"
  }
}
```

Then, in your [Operations](/docs/language/features#queries-and-actions-aka-operations) or [setupFn](/docs/language/features#setupfn-extimport-optional) (or any other NodeJS code), you can submit work to be done:
```js
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

const submittedJob = await mySpecialJob.submit({ job: "args" })
console.log(await submittedJob.pgBoss.details())

// Or, if you'd prefer it to execute in the future, just add a .delay().
// It takes a number of seconds, Date, or ISO date string.
await mySpecialJob.delay(10).submit({ job: "args" })
```

And that is it! Your job will be executed by the job executor (pg-boss, in this case) as if you called `foo({ job: "args" })`.

### Recurring jobs

If you have work that needs to be done on some recurring basis, you can add a `schedule` to your job declaration:

```css  {6-9} title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@ext/jobs/bar.js"
  },
  schedule: {
    cron: "0 * * * *",
    args: {=json { "job": "args" } json=} // optional
  }
}
```

In this example, you do _not_ need to invoke anything in JavaScript. You can imagine `foo({ "job": "args" })` getting automatically scheduled and invoked for you every hour.

### Fully specified example
Both `perform` and `schedule` accept `executorOptions`, which we pass directly to the named job executor when you submit jobs. In this example, the scheduled job will have a `retryLimit` set to 0, as `schedule` overrides any similar property from `perform`. Lastly, we add an entity to pass in via the context argument to `perform.fn`.

```css
job mySpecialJob {
  executor: PgBoss,
  entities: [Task],
  perform: {
    fn: import { foo } from "@ext/jobs/bar.js",
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
  }
}
```

### Fields

#### `executor: JobExecutor` (required)
`PgBoss` is currently our only job executor, and is recommended for low-volume production use cases. It requires your `app.db.system` to be `PostgreSQL`.

####  `perform: dict` (required)

  - ##### `fn: fn` (required)
  An `async` JavaScript function of work to be performed. It receives a JSON value as the first argument and context containing any declared entities as the second. Here is a sample signature:

  ```js
  export async function foo(args, context) {
    // Can reference context.entities.Task, for example.
  }
  ```
  
  - ##### `executorOptions: dict` (optional)
  Executor-specific default options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. These can be overridden during invocation with `submit()` or in a `schedule`.

    - ##### `pgBoss: JSON` (optional)
    See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#sendname-data-options).

#### `schedule: dict` (optional)
  
  - ##### `cron: string` (required)
  A 5-placeholder format cron expression string. See rationale for minute-level precision [here](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#scheduling).
  
  - ##### `args: JSON` (optional)
  The arguments to pass to the `perform.fn` function when invoked.
    
  - ##### `executorOptions: dict` (optional)
  Executor-specific options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. The `perform.executorOptions` are the default options, and `schedule.executorOptions` can override/extend those.

    - ##### `pgBoss: JSON` (optional)
    See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#sendname-data-options).

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
  - `details()`: pg-boss specific job detail information. [Reference](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#getjobbyidid)
  - `cancel()`: attempts to cancel a job. [Reference](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#cancelid)
  - `resume()`: attempts to resume a canceled job. [Reference](https://github.com/timgit/pg-boss/blob/7.2.1/docs/readme.md#resumeid)

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
    externalAuthAssociationEntity: SocialLogin,
    methods: {
      usernameAndPassword: {},
      google: {}
    },
    onAuthFailedRedirectTo: "/someRoute"
  }
}
```

`app.auth` is a dictionary with following fields:

#### `userEntity: entity` (required)
Entity which represents the user (sometimes also referred to as *Principal*).

#### `externalAuthAssociationEntity: entity` (optional)
Entity which associates a user with some external authentication provider.

#### `methods: dict` (required)
List of authentication methods that Wasp app supports. Currently supported methods are:
* `usernameAndPassword`: Provides support for authentication with a username and password. See [here](#username-and-password) for more.
* `google`: Provides support for login via Google accounts. See [here](#google) for more.

#### `onAuthFailedRedirectTo: String` (required)
Path where an unauthenticated user will be redirected to if they try to access a private page (which is declared by setting `authRequired: true` for a specific page).
Check out this [section of our Todo app tutorial](/docs/tutorials/todo-app/auth#updating-main-page-to-check-if-user-is-authenticated) to see an example of usage.

#### `onAuthSucceededRedirectTo: String` (optional)
Path where a successfully authenticated user will be sent upon successful login/signup.
Default value is "/".

### Username and Password

`usernameAndPassword` authentication method makes it possible to signup/login into the app by using a username and password.
This method requires that `userEntity` specified in `auth` contains `username: string` and `password: string` fields.

We provide basic validations out of the box, which you can customize as shown below. Default validations are:
- `username`: non-empty
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
        data: { username: 'waspeteer', password: 'this will be hashed!' }
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
  data: { username: 'waspeteer', password: 'this will be hashed!' },
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

### `login()`
An action for logging in the user.
```js
login(username, password)
```
#### `username: string`
Username of the user logging in.

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

### Google

`google` authentication makes it possible to use Google's OAuth 2.0 service to sign Google users into your app. To enable it, add `google: {}` to your `auth.methods` dictionary to use it with default settings. If you require custom configuration setup or user entity field assignment, you can override the defaults.

This method requires that `externalAuthAssociationEntity` specified in `auth` [described here](features#externalauthassociationentity).
#### Default settings
- Configuration:
  - By default, we expect you to set two environment variables in order to use Google authentication:
    - `GOOGLE_CLIENT_ID`
    - `GOOGLE_CLIENT_SECRET`
  - These can be obtained in your Google Cloud Console project dashboard. See [here](/docs/integrations/google#google-auth) for more.
- Sign in:
  - When a user signs in for the first time, we will create a new User account and link it to their Google account for future logins. The `username` will default to a random dictionary phrase that does not exist in the database, like "nice-blue-horse-27160".
- Here is a link to the default implementations: https://github.com/wasp-lang/wasp/blob/main/waspc/data/Generator/templates/server/src/routes/auth/passport/google/googleDefaults.js

#### Overrides
If you require modifications to the above, you can add one or more of the following to your `auth.methods.google` dictionary:

```js
  auth: {
    userEntity: User,
    externalAuthAssociationEntity: SocialLogin,
    methods: {
      google: {
        configFn: import { config } from "@ext/auth/google.js",
        getUserFieldsFn: import { getUserFields } from "@ext/auth/google.js"
      }
    },
    ...
  }
```

- `configFn`: This function should return an object with the following shape:
  ```js
  export function config() {
    return {
      clientId, // look up from env or elsewhere,
      clientSecret, // look up from env or elsewhere,
      scope: ['profile'] // must include at least 'profile'
    }
  }
  ```
- `getUserFieldsFn`: This function should return user fields. The context contains a User entity for DB access, and the args are what the OAuth provider responds with. Here is how you could generate a username based on the Google display name. In your model, you could choose to add more attributes and set additional information.
  ```js
  import { generateAvailableUsername } from '@wasp/core/auth.js'

  export async function getUserFields(_context, args) {
    const username = await generateAvailableUsername(args.profile.displayName.split(' '), { separator: '.' })
    return { username }
  }
  ```
  - `generateAvailableUsername` takes an array of Strings and an optional separator, and generates a string ending with a random number that is not yet in the database. For example, the above could produce something like "Jim.Smith.3984" for a Google user Jim Smith.

#### UI helpers

To use the Google sign-in button or URL on your login page, do either of the following:

```js
...
import { GoogleSignInButton, googleSignInUrl } from '@wasp/auth/buttons/Google'

const Login = () => {
  return (
    <>
      ...

      <GoogleSignInButton/>
      {/* or */}
      <a href={googleSignInUrl}>Sign in with Google</a>
    </>
  )
}

export default Login
```

You can set the height of the button by setting a prop (e.g., `<Google height={25}/>`), which defaults to 40px.


### `externalAuthAssociationEntity`
Anytime an authentication method is used that relies on an external authorization provider, for example Google, we require a `externalAuthAssociationEntity` specified in `auth` contains at least the following highlighted fields:

```css {4,11,16-21}
...
  auth: {
    userEntity: User,
    externalAuthAssociationEntity: SocialLogin,
...

entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
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

## Client configuration

You can configure the client using the `client` field inside the `app`
declaration, 

```c
app MyApp {
  title: "My app",
  // ...
  client: {
    setupFn: import mySetupFunction from "@ext/myClientSetupCode.js"
  }
}
```

`app.client` is a dictionary with the following fields:

#### `setupFn: ExtImport` (optional)

`setupFn` declares a JavaScript function that Wasp executes on the client
before everything else. It is expected to be asynchronous, and
Wasp will await its completion before rendering the page. The function takes no
arguments, and its return value is ignored.

You can use this function to perform any custom setup (e.g., setting up
client-side periodic jobs).

Here's a dummy example of such a function:

```js title="ext/myClientSetupCode.js"
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


```js title="ext/myClientSetupCode.js"
import { configureQueryClient } from '@wasp/queries'

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

Make sure to pass in an object expected by the `QueryClient`'s construcor, as
explained in
[_react-query_'s docs](https://react-query.tanstack.com/reference/QueryClient).

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

## Database configuration

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
When using `PostgreSQL` as your database (`app: { db: { system: PostgreSQL } }`), you will need to spin up a postgres database on your own so it runs during development (when running `wasp start` or doing `wasp db ...` commands) and you will need to provide Wasp with `DATABASE_URL` environment variable that Wasp will use to connect to it.

One of the easiest ways to run a PostgreSQL database on your own is by spinning up [postgres docker](https://hub.docker.com/_/postgres) container when you need it with the following shell command:
```
docker run \
  --rm \
  --publish 5432:5432 \
  -v my-app-data:/var/lib/postgresql/data \
  --env POSTGRES_PASSWORD=devpass1234 \
  postgres
```

:::note
The password you provide via `POSTGRES_PASSWORD` is relevant only for the first time when you run that docker command, when database is set up for the first time. Consequent runs will ignore the value of `POSTGRES_PASSWORD` and will just use the password that was initially set. This is just how postgres docker works.
:::

The easiest way to provide the needed `DATABASE_URL` environment variable is by adding the following line to the [.env](https://wasp-lang.dev/docs/language/features#env) file in the root dir of your Wasp project (if that file doesn't yet exist, create it):
```
DATABASE_URL=postgresql://postgres:devpass1234@localhost:5432/postgres
```

### Migrating from SQLite to PostgreSQL
To run Wasp app in production, you will need to switch from `SQLite` to `PostgreSQL`.
1. Set `app.db.system` to `PostgreSQL` and set `DATABASE_URL` env var accordingly (as described [above](/docs/language/features#postgresql)).
2. Delete old migrations, since they are SQLite migrations and can't be used with PostgreSQL: `rm -r migrations/`.
3. Run `wasp db migrate-dev` to apply new changes and create new, initial migration. You will need to have your postgres database running while doing this (check [above](/docs/language/features#postgresql) for easy way to get it running).
