# Interface: Job

A background job. Can be submitted ad-hoc or run on a recurring schedule.

Create one with the [job](../functions/job.md) constructor.

See [Recurring Jobs](https://wasp.sh/docs/advanced/jobs).

## Extends

- `BaseSpecElement`\<`"job"`\>

## Properties

### entities?

> `optional` **entities?**: `string`[]

Entities the worker operates on. Wasp injects a Prisma delegate for
each one into the worker's `context.entities`.

This works like entity access in queries and actions. See
[Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).

***

### executor

> **executor**: `"PgBoss"`

Executor backing this job.

Currently only `"PgBoss"` is available.

***

### fn

> **fn**: [`Reference`](../type-aliases/Reference.md)\<`AnyFunction`\>

Reference to the job's NodeJS implementation. It receives the submitted
args and a context containing the declared entities.

See [Jobs documentation](https://wasp.sh/docs/advanced/jobs#worker-api) for more details.

***

### kind

> **kind**: `"job"`

The internal Wasp type of a [SpecElement](../type-aliases/SpecElement.md). Used by the compiler.
You should not set this field directly, instead use the dedicated constructors.

#### Inherited from

`BaseSpecElement.kind`

***

### performExecutorOptions?

> `optional` **performExecutorOptions?**: [`ExecutorOptions`](ExecutorOptions.md)

Executor-specific default options used when submitting the job.

These options are passed through to the executor and can be overridden
when submitting the job or by [Schedule.executorOptions](Schedule.md#executoroptions) for
scheduled runs. For example, with PgBoss this can set retry limits,
expiration, or priority.

#### Default

```ts
{}
```

***

### schedule?

> `optional` **schedule?**: [`Schedule`](Schedule.md)

Cron schedule that runs the job automatically.
