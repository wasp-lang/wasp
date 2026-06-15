# Interface: Schedule

Cron schedule for a [Job](Job.md).

## Properties

### args?

> `optional` **args?**: `object`

Arguments passed to the worker function on each scheduled run.

***

### cron

> **cron**: `string`

Five-field cron expression (e.g. `"0 * * * *"` for hourly).

Wasp supports minute-level precision.

See pg-boss's [scheduling docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#scheduling)
for the rationale. Use [Crontab Guru](https://crontab.guru/#0_*_*_*_*)
to build cron expressions.

***

### executorOptions?

> `optional` **executorOptions?**: [`ExecutorOptions`](ExecutorOptions.md)

Executor-specific options applied only to scheduled runs. These override
or extend [Job.performExecutorOptions](Job.md#performexecutoroptions).
