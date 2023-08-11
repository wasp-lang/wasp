---
title: Recurring Jobs
---

import { Required } from '@site/src/components/Required.tsx'

If you have server tasks that you do not want to handle as part of the normal request-response cycle, Wasp allows you to make that function a `job` and it will gain some "superpowers."

Jobs will:
  * persist between server restarts
  * can be retried if they fail
  * can be delayed until the future
  * can have a recurring schedule!

Some examples where you may want to use a `job` on the server include sending an email, making an HTTP request to some external API, or doing some nightly calculations.

### Basic Job Definition and Usage

To declare a `job` in Wasp, simply add a declaration with a reference to an `async` function, like the following:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@server/workers/bar.js"
  }
}
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
job mySpecialJob {
  executor: PgBoss,
  perform: {
    fn: import { foo } from "@server/workers/bar.js"
  }
}
```
</TabItem>
</Tabs>

Then, in your [Operations](/docs/language/features#queries-and-actions-aka-operations) or [setupFn](/docs/language/features#setupfn-serverimport-optional) (or any other NodeJS code), you can submit work to be done:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="someAction.js"
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

const submittedJob = await mySpecialJob.submit({ job: "args" })
console.log(await submittedJob.pgBoss.details())

// Or, if you'd prefer it to execute in the future, just add a .delay().
// It takes a number of seconds, Date, or ISO date string.
await mySpecialJob.delay(10).submit({ job: "args" })
```
</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="someAction.ts"
import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

const submittedJob = await mySpecialJob.submit({ job: "args" })
console.log(await submittedJob.pgBoss.details())

// Or, if you'd prefer it to execute in the future, just add a .delay().
// It takes a number of seconds, Date, or ISO date string.
await mySpecialJob.delay(10).submit({ job: "args" })
```
</TabItem>
</Tabs>

And that is it! Your job will be executed by the job executor (pg-boss, in this case) as if you called `foo({ job: "args" })`.

Note that in our example, `foo` takes an argument, but this does not always have to be the case. It all depends on how you've implemented your worker function.

### Recurring Jobs

If you have work that needs to be done on some recurring basis, you can add a `schedule` to your job declaration:

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

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
</TabItem>
<TabItem value="ts" label="TypeScript">

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
</TabItem>
</Tabs>

In this example, you do _not_ need to invoke anything in JavaScript. You can imagine `foo({ job: "args" })` getting automatically scheduled and invoked for you every hour.

### Fully Specified Example
Both `perform` and `schedule` accept `executorOptions`, which we pass directly to the named job executor when you submit jobs. In this example, the scheduled job will have a `retryLimit` set to 0, as `schedule` overrides any similar property from `perform`. Lastly, we add an entity to pass in via the context argument to `perform.fn`.

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

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
</TabItem>
<TabItem value="ts" label="TypeScript">

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
</TabItem>
</Tabs>

## API Reference

### Fields

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```wasp title="main.wasp"
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
</TabItem>
<TabItem value="ts" label="TypeScript">

```wasp title="main.wasp"
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
</TabItem>
</Tabs>

The `job` declaration has the following fields:

-  `executor: JobExecutor` <Required />

  :::note Job executors
  Our jobs need job executors to handle the _scheduling, monitoring, and execution_.

  Wasp allows you to choose which job executor will be used to execute a specific job that you define, which affects some of the finer details of how jobs will behave and how they can be further configured. Each job executor has its pros and cons, which we will explain in more detail below, so you can pick the one that best suits your needs.
  :::

  `PgBoss` is currently our only job executor, and is recommended for low-volume production use cases. It requires your `app.db.system` to be `PostgreSQL`.

  We have selected [pg-boss](https://github.com/timgit/pg-boss/) as our first job executor to handle the low-volume, basic job queue workloads many web applications have. By using PostgreSQL (and [SKIP LOCKED](https://www.2ndquadrant.com/en/blog/what-is-select-skip-locked-for-in-postgresql-9-5/)) as its storage and synchronization mechanism, it allows us to provide many job queue pros without any additional infrastructure or complex management.

  :::info
  Keep in mind that pg-boss jobs run alongside your other server-side code, so they are not appropriate for CPU-heavy workloads. Additionally, some care is required if you modify scheduled jobs. Please see pg-boss details below for more information.

  <details>
  <summary>pg-boss details</summary>

    pg-boss provides many useful features, which can be found [here](https://github.com/timgit/pg-boss/blob/8.4.2/README.md).

    When you add pg-boss to a Wasp project, it will automatically add a new schema to your database called `pgboss` with some internal tracking tables, including `job` and `schedule`. pg-boss tables have a `name` column in most tables that will correspond to your `job` identifier. Additionally, these tables maintain arguments, states, return values, retry information, start and expiration times, and other metadata required by pg-boss.

    If you need to customize the creation of the pg-boss instance, you can set an environment variable called `PG_BOSS_NEW_OPTIONS` to a stringified JSON object containing [these initialization parameters](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#newoptions). **NOTE**: Setting this overwrites all Wasp defaults, so you must include database connection information as well.

    ### pg-boss considerations
    - Wasp starts pg-boss alongside your web server's application, where both are simultaneously operational. This means that jobs running via pg-boss and the rest of the server logic (like Operations) share the CPU, therefore you should avoid running CPU-intensive tasks via jobs.
      - Wasp does not (yet) support independent, horizontal scaling of pg-boss-only applications, nor starting them as separate workers/processes/threads.
    - The job name/identifier in your `.wasp` file is the same name that will be used in the `name` column of pg-boss tables. If you change a name that had a `schedule` associated with it, pg-boss will continue scheduling those jobs but they will have no handlers associated, and will thus become stale and expire. To resolve this, you can remove the applicable row from the `schedule` table in the `pgboss` schema of your database.
      - If you remove a `schedule` from a job, you will need to do the above as well.
    - If you wish to deploy to Heroku, you need to set an additional environment variable called `PG_BOSS_NEW_OPTIONS` to `{"connectionString":"<REGULAR_HEROKU_DATABASE_URL>","ssl":{"rejectUnauthorized":false}}`. This is because pg-boss uses the `pg` extension, which does not seem to connect to Heroku over SSL by default, which Heroku requires. Additionally, Heroku uses a self-signed cert, so we must handle that as well.
    - https://devcenter.heroku.com/articles/connecting-heroku-postgres#connecting-in-node-js

  </details>

  :::

- `perform: dict` <Required />

  - `fn: ServerImport` <Required />

    An `async` JavaScript function of work to be performed. Since Wasp executes jobs on the server, you must import it from `@server`. The function receives a first argument which may be passed when the job is called, as well as the context containing any declared entities as the second (this is passed automatically by Wasp). Here is a sample signature:

    <Tabs groupId="js-ts">
    <TabItem value="js" label="JavaScript">
    
    ```js title="bar.js"
    export async function foo(args, context) {
      // Can reference context.entities.Task, for example.
    }
    ```
    </TabItem>
    <TabItem value="ts" label="TypeScript">
    
    ```ts title="bar.ts"
    export async function foo(args, context) {
      // Can reference context.entities.Task, for example.
    }
    ```
    </TabItem>
    </Tabs>

  - `executorOptions: dict`

    Executor-specific default options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. These can be overridden during invocation with `submit()` or in a `schedule`.

    - `pgBoss: JSON`

      See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options).

- `schedule: dict`

  - `cron: string` <Required />

    A 5-placeholder format cron expression string. See rationale for minute-level precision [here](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#scheduling).

    _If you need help building cron expressions, Check out_ <em>[Crontab guru](https://crontab.guru/#0_*_*_*_*).</em>

  - `args: JSON`
  
    The arguments to pass to the `perform.fn` function when invoked.

  - `executorOptions: dict`

    Executor-specific options to use when submitting jobs. These are passed directly through and you should consult the documentation for the job executor. The `perform.executorOptions` are the default options, and `schedule.executorOptions` can override/extend those.

    - `pgBoss: JSON`
    
      See the docs for [pg-boss](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options).

- `entities: [Entity]`
  
  A list of entities you wish to use inside your Job (similar to [Queries and Actions](/docs/data-model/operations/queries#using-entities-in-queries)).

### JavaScript API

- `import`

  <Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">

  ```js title="someAction.js"
  import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'
  ```
  </TabItem>
  <TabItem value="ts" label="TypeScript">

  ```ts title="someAction.ts"
  import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'
  ```
  </TabItem>
  </Tabs>

- `submit(jobArgs, executorOptions)`
  - `jobArgs: JSON`
  - `executorOptions: JSON`

  Submits a `job` to be executed by an executor, optionally passing in a JSON job argument your job handler function will receive, and executor-specific submit options.

 <Tabs groupId="js-ts">
 <TabItem value="js" label="JavaScript">
 
  ```js title="someAction.js"
  const submittedJob = await mySpecialJob.submit({ job: "args" })
  ```
 </TabItem>
 <TabItem value="ts" label="TypeScript">
 
  ```js title="someAction.ts"
  const submittedJob = await mySpecialJob.submit({ job: "args" })
  ```
 </TabItem>
 </Tabs>

- `delay(startAfter)`
  - `startAfter: int | string | Date` <Required />

  Delaying the invocation of the job handler. The delay can be one of:
    - Integer: number of seconds to delay. [Default 0]
    - String: ISO date string to run at.
    - Date: Date to run at.

 <Tabs groupId="js-ts">
 <TabItem value="js" label="JavaScript">
 
  ```js title="someAction.js"
  const submittedJob = await mySpecialJob.delay(10).submit({ job: "args" }, { "retryLimit": 2 })
  ```
 </TabItem>
 <TabItem value="ts" label="TypeScript">
 
  ```ts title="someAction.ts"
  const submittedJob = await mySpecialJob.delay(10).submit({ job: "args" }, { "retryLimit": 2 })
  ```
 </TabItem>
 </Tabs>

#### Tracking
The return value of `submit()` is an instance of `SubmittedJob`, which minimally contains:
- `jobId`: A getter returning the UUID String ID for the job in that executor.
- `jobName`: A getter returning the name of the job you used in your `.wasp` file.
- `executorName`: A getter returning a Symbol of the name of the job executor.
  - For pg-boss, you can import a Symbol from: `import { PG_BOSS_EXECUTOR_NAME } from '@wasp/jobs/core/pgBoss/pgBossJob.js'` if you wish to compare against `executorName`.

There are also some namespaced, job executor-specific objects.

- For pg-boss, you may access: `pgBoss`
  - **NOTE**: no arguments are necessary, as we already applied the `jobId` in the available functions.
  - `details()`: pg-boss specific job detail information. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#getjobbyidid)
  - `cancel()`: attempts to cancel a job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#cancelid)
  - `resume()`: attempts to resume a canceled job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#resumeid)