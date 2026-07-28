# Function: job()

> **job**(`fn`, `config`): [`Job`](../interfaces/Job.md)

Creates a [Job](../interfaces/Job.md) definition.

Jobs are background tasks that persist across server restarts, can be
retried on failure, delayed, and scheduled with cron. Pass the worker
function as the first argument and configure the executor and schedule
in `config`.

See [Recurring Jobs](https://wasp.sh/docs/advanced/jobs).

## Parameters

### fn

[`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

The async function that performs the job's work. It receives the
  submitted args and a context containing the declared entities.

<!-- Used as a shared snippet in multiple different files with @include helper. -->

Use [reference imports](https://wasp.sh/docs/general/spec#reference-imports)
when passing values from your app into the Wasp spec. If a direct import is not
practical, use [ref](ref.md) as a fallback.

### config

Required `executor` and optional `schedule`, `entities`,
  and `performExecutorOptions`.

#### entities?

`string`[]

Entities the worker operates on. Wasp injects a Prisma delegate for
each one into the worker's `context.entities`.

This works like entity access in queries and actions. See
[Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).

#### executor

`"PgBoss"`

Executor backing this job.

Currently only `"PgBoss"` is available.

#### performExecutorOptions?

[`ExecutorOptions`](../interfaces/ExecutorOptions.md)

Executor-specific default options used when submitting the job.

These options are passed through to the executor and can be overridden
when submitting the job or by [Schedule.executorOptions](../interfaces/Schedule.md#executoroptions) for
scheduled runs. For example, with PgBoss this can set retry limits,
expiration, or priority.

**Default**

```ts
{}
```

#### schedule?

[`Schedule`](../interfaces/Schedule.md)

Cron schedule that runs the job automatically.

## Returns

[`Job`](../interfaces/Job.md)

## Example

```ts
import { job } from '@wasp.sh/spec'
import { foo } from './src/jobs/bar' with { type: 'ref' }

job(foo, {
  executor: 'PgBoss',
  entities: ['Task'],
  schedule: { cron: '0 * * * *' },
})
```
