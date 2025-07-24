---
title: Recurring Jobs
---

import { Required } from '@site/src/components/Tag'
import { ShowForTs, ShowForJs } from '@site/src/components/TsJsHelpers'

In most web apps, users send requests to the server and receive responses with some data. When the server responds quickly, the app feels responsive and smooth.

What if the server needs extra time to fully process the request? This might mean sending an email or making a slow HTTP request to an external API. In that case, it's a good idea to respond to the user as soon as possible and do the remaining work in the background.

Wasp supports background jobs that can help you with this:

- Jobs persist between server restarts,
- Jobs can be retried if they fail,
- Jobs can be delayed until a future time,
- Jobs can have a recurring schedule.

## Using Jobs

### Job Definition and Usage

Let's write an example Job that will print a message to the console and return a list of tasks from the database.

1. Start by creating a Job declaration in your `.wasp` file:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    job mySpecialJob {
      executor: PgBoss,
      perform: {
        fn: import { foo } from "@src/workers/bar"
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
        fn: import { foo } from "@src/workers/bar"
      },
      entities: [Task],
    }
    ```
  </TabItem>
</Tabs>

2. After declaring the Job, implement its worker function:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="src/workers/bar.js"
    export const foo = async ({ name }, context) => {
      console.log(`Hello ${name}!`)
      const tasks = await context.entities.Task.findMany({})
      return { tasks }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="src/workers/bar.ts"
    import { type MySpecialJob } from 'wasp/server/jobs'
    import { type Task } from 'wasp/entities'

    type Input = { name: string; }
    type Output = { tasks: Task[]; }

    export const foo: MySpecialJob<Input, Output> = async ({ name }, context) => {
      console.log(`Hello ${name}!`)
      const tasks = await context.entities.Task.findMany({})
      return { tasks }
    }
    ```
  </TabItem>
</Tabs>

:::info The worker function
The worker function must be an `async` function. The function's return value represents the Job's result.

The worker function accepts two arguments:

- `args`: The data passed into the job when it's submitted.
- `context: { entities }`: The context object containing entities you put in the Job declaration.
  :::

<ShowForTs>
  `MySpecialJob`  is a generic type Wasp generates to help you  correctly type the Job's worker function, ensuring type information about the function's arguments and return value. Read more about type-safe jobs in the [Javascript API section](#javascript-api).
</ShowForTs>

3. After successfully defining the job, you can submit work to be done in your [Operations](../data-model/operations/overview) or [setupFn](../project/server-config#setup-function) (or any other NodeJS code):

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="someAction.js"
    import { mySpecialJob } from 'wasp/server/jobs'

    const submittedJob = await mySpecialJob.submit({ job: "Johnny" })

    // Or, if you'd prefer it to execute in the future, just add a .delay().
    // It takes a number of seconds, Date, or ISO date string.
    await mySpecialJob
      .delay(10)
      .submit({ name: "Johnny" })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="someAction.ts"
    import { mySpecialJob } from 'wasp/server/jobs'

    const submittedJob = await mySpecialJob.submit({ job: "Johnny" })

    // Or, if you'd prefer it to execute in the future, just add a .delay().
    // It takes a number of seconds, Date, or ISO date string.
    await mySpecialJob
      .delay(10)
      .submit({ name: "Johnny" })
    ```
  </TabItem>
</Tabs>

And that's it. Your job will be executed by `PgBoss` as if you called `foo({ name: "Johnny" })`.

In our example, `foo` takes an argument, but passing arguments to jobs is not a requirement. It depends on how you've implemented your worker function.

### Recurring Jobs

If you have work that needs to be done on some recurring basis, you can add a `schedule` to your job declaration:

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp {6-9} title="main.wasp"
    job mySpecialJob {
      executor: PgBoss,
      perform: {
        fn: import { foo } from "@src/workers/bar"
      },
      schedule: {
        cron: "0 * * * *",
        args: {=json { "job": "args" } json=} // optional
      }
    }
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```wasp {6-9} title="main.wasp"
    job mySpecialJob {
      executor: PgBoss,
      perform: {
        fn: import { foo } from "@src/workers/bar"
      },
      schedule: {
        cron: "0 * * * *",
        args: {=json { "job": "args" } json=} // optional
      }
    }
    ```
  </TabItem>
</Tabs>

In this example, you _don't_ need to invoke anything in <ShowForJs>JavaScript</ShowForJs><ShowForTs>Typescript</ShowForTs>. You can imagine `foo({ job: "args" })` getting automatically scheduled and invoked for you every hour.

<!-- TODO: write this piece after we complete https://github.com/wasp-lang/wasp/issues/1412 -->

<!-- ### Getting the Job's Result

When you submit a job, you get a `SubmittedJob` object back. It has a `jobId` field, which you can use to get the job's result. -->

## Job executors

Wasp supports Jobs through the use of **job executors**. A job executor is responsible for handling the scheduling, monitoring, and execution of jobs.

Currently, Wasp only has support for one job executor, `PgBoss`.

### `PgBoss` {#pgboss}

[`PgBoss`](https://github.com/timgit/pg-boss/tree/8.4.2) is a lightweight job queue built on top of PostgreSQL. It is suitable for low-volume production use cases and does not require any additional infrastructure or complex management. By using PostgreSQL (and [SKIP LOCKED](https://www.2ndquadrant.com/en/blog/what-is-select-skip-locked-for-in-postgresql-9-5/)) as its storage and synchronization mechanism, you get many benefits of a traditional job queue, on top of your existing Postgres database.

#### Requirements

`PgBoss` requires that your database provider is set to `"postgresql"` in your `schema.prisma` file. Read more about setting the provider [here](../data-model/databases.md#postgresql).

#### Limitations

`PgBoss` runs together with your web server, whenever it is up. This means that it is not a separate process or service, but rather a part of your web server's application. As such, it is not suitable for CPU-heavy workloads, as it shares the CPU with your web server's application logic.

The `PgBoss` executor in Wasp does not (yet) support independent, horizontal scaling of pg-boss-only applications, nor starting them as separate workers/processes/threads. This means that your server must be running whenever you want to process jobs. If you need to scale your job processing, you will need to run multiple instances of your web server, each with its own `PgBoss` instance.

#### Customization {#pg_boss_new_options}

If you need to customize the creation of the `PgBoss` instance, you can set an environment variable called `PG_BOSS_NEW_OPTIONS` to a stringified JSON object containing the initialization parameters. See the [pg-boss documentation](https://github.com/timgit/pg-boss/tree/8.4.2/docs#newoptions).

Please note that setting `PG_BOSS_NEW_OPTIONS` environment variable overwrites all Wasp defaults, so you must include the `connectionString` parameter inside it as well.

For example, to set the connection string and change the job archival and deletion settings, you can set the environment variable like this:

```bash
# In an .env file
PG_BOSS_NEW_OPTIONS={"connectionString":"postgresql://user:password@server:5432/database","archiveCompletedAfterSeconds":86400,"deleteAfterDays":30,"maintenanceIntervalMinutes":5}

# In the shell
PG_BOSS_NEW_OPTIONS='{"connectionString":"postgresql://user:password@server:5432/database","archiveCompletedAfterSeconds":86400,"deleteAfterDays":30,"maintenanceIntervalMinutes":5}'
```

You can read more about escaping JSON in environment variables in the [JSON Env Vars documentation](../project/env-vars.md#json-env-vars).

#### Database setup

:::tip You don't need to set up the database manually

When using `PgBoss`, the database setup is automatically taken care of by the Wasp server, and doesn't need to be reflected in your schemas or migrations. The following information is given for your reference, and is explained in more detail in [the `PgBoss` documentation](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md).

:::

All job data will be stored in a separate database schema called `pgboss`. It has some internal tracking tables, such as `job`, `archive`, and `schedule`. `PgBoss` tables have a `name` column in most tables that will correspond to your Job identifier. Additionally, these tables maintain arguments, states, return values, retry information, start and expiration times, and other metadata required by `PgBoss`.

#### Known issues

- **Renaming scheduled jobs**

    The job name/identifier in your `.wasp` file is the same name that will be used in the `name` column of `pgboss` tables. If you change a name that had a `schedule` associated with it, pg-boss will continue scheduling those jobs but they will have no handlers associated, and will thus become stale and expire. To resolve this, you can remove the applicable row from the `pgboss.schedule` table.

    For example, if you renamed a job from `emailReminder` to `sendEmailReminder`, you would need to remove the old scheduled job with the following SQL query:

    ```sql
    BEGIN;
    DELETE FROM pgboss.schedule WHERE name = 'emailReminder';
    COMMIT;
    ```

    **Important:** Only modify the database directly if you're comfortable with SQL operations. If you're unsure, consider keeping the old job name or restarting with a fresh database in development.

#### Job data retention and cleanup

By default, `PgBoss` keeps job data for 12 hours after completion or failure. After that, it moves the data to an archive table, where it is kept for 7 days before being deleted. If you want to change this behavior, you can configure the `PG_BOSS_NEW_OPTIONS` environment variable to set custom values for job archival ([`archivedCompletedAfterSeconds`/`archiveFailedAfterSeconds`](https://github.com/timgit/pg-boss/tree/8.4.2/docs#newoptions:~:text=v1%22%20or%20%22v4%22-,archiveCompletedAfterSeconds,-Specifies%20how%20long)) and removal ([`deleteAfterSeconds`/`deleteAfterMinutes`/etc](https://github.com/timgit/pg-boss/tree/8.4.2/docs#newoptions:~:text=the%20skew%20warnings.-,Archive%20options,-When%20jobs%20in)).

```bash
PG_BOSS_NEW_OPTIONS={"connectionString":"...your postgress connection url...","archiveCompletedAfterSeconds":86400,"deleteAfterDays":30,"maintenanceIntervalMinutes":5}
```

## API Reference

### Declaring Jobs

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```wasp title="main.wasp"
    job mySpecialJob {
      executor: PgBoss,
      perform: {
        fn: import { foo } from "@src/workers/bar",
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
        fn: import { foo } from "@src/workers/bar",
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

The Job declaration has the following fields:

- `executor: JobExecutor` <Required />

  The job executor to use for this job. Currently, the only supported executor is [`PgBoss`](#pgboss).

- `perform: dict` <Required />

  - `fn: ExtImport` <Required />

    - An `async` function that performs the work. Since Wasp executes Jobs on the server, the import path must lead to a NodeJS file.
    - It receives the following arguments:
      - `args: Input`: The data passed to the job when it's submitted.
      - `context: { entities: Entities }`: The context object containing any declared entities.

    Here's an example of a `perform.fn` function:

    <Tabs groupId="js-ts">
      <TabItem value="js" label="JavaScript">
        ```js title="src/workers/bar.js"
        export const foo = async ({ name }, context) => {
          console.log(`Hello ${name}!`)
          const tasks = await context.entities.Task.findMany({})
          return { tasks }
        }
        ```
      </TabItem>

      <TabItem value="ts" label="TypeScript">
        ```ts title="src/workers/bar.ts"
        import { type MySpecialJob } from 'wasp/server/jobs'

        type Input = { name: string; }
        type Output = { tasks: Task[]; }

        export const foo: MySpecialJob<Input, Output> = async ({ name }, context) => {
          console.log(`Hello ${name}!`)
          const tasks = await context.entities.Task.findMany({})
          return { tasks }
        }
        ```

        Read more about type-safe jobs in the [Javascript API section](#javascript-api).
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

  A list of entities you wish to use inside your Job (similar to [Queries and Actions](../data-model/operations/queries#using-entities-in-queries)).

### JavaScript API

- Importing a Job:

  <Tabs groupId="js-ts">
    <TabItem value="js" label="JavaScript">
      ```js title="someAction.js"
      import { mySpecialJob } from 'wasp/server/jobs'
      ```
    </TabItem>

    <TabItem value="ts" label="TypeScript">
      ```ts title="someAction.ts"
      import { mySpecialJob, type MySpecialJob } from 'wasp/server/jobs'
      ```

      :::info Type-safe jobs
      Wasp generates a generic type for each Job declaration, which you can use to type your `perform.fn` function. The type is named after the job declaration, and is available in the `wasp/server/jobs` module. In the example above, the type is `MySpecialJob`.

      The type takes two type arguments:

      - `Input`: The type of the `args` argument of the `perform.fn` function.
      - `Output`: The type of the return value of the `perform.fn` function.
        :::
    </TabItem>
  </Tabs>

- `submit(jobArgs, executorOptions)`

  - `jobArgs: Input`
  - `executorOptions: object`

  Submits a Job to be executed by an executor, optionally passing in a JSON job argument your job handler function receives, and executor-specific submit options.

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

  - Integer: number of seconds to delay. \[Default 0]
  - String: ISO date string to run at.
  - Date: Date to run at.

<Tabs groupId="js-ts">
  <TabItem value="js" label="JavaScript">
    ```js title="someAction.js"
    const submittedJob = await mySpecialJob
      .delay(10)
      .submit({ job: "args" }, { "retryLimit": 2 })
    ```
  </TabItem>

  <TabItem value="ts" label="TypeScript">
    ```ts title="someAction.ts"
    const submittedJob = await mySpecialJob
      .delay(10)
      .submit({ job: "args" }, { "retryLimit": 2 })
    ```
  </TabItem>
</Tabs>

#### Tracking

The return value of `submit()` is an instance of `SubmittedJob`, which has the following fields:

- `jobId`: The ID for the job in that executor.
- `jobName`: The name of the job you used in your `.wasp` file.
- `executorName`: The Symbol of the name of the job executor.

There are also some namespaced, job executor-specific objects.

- For pg-boss, you may access: `pgBoss`
  - `details()`: pg-boss specific job detail information. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#getjobbyidid)
  - `cancel()`: attempts to cancel a job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#cancelid)
  - `resume()`: attempts to resume a canceled job. [Reference](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#resumeid)
