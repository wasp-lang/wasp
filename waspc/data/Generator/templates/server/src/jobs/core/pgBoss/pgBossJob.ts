import PgBoss from 'pg-boss'
import { pgBossStarted } from './pgBoss.js'
import { Job, SubmittedJob } from '../job.js'
import type { JSONValue, JSONObject } from '../../../_types/serialization.js'
import { PrismaDelegate } from '../../../_types/index.js'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

/**
 * Creates an instance of PgBossJob and initializes the PgBoss executor by registering this job function.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 */
export function createJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
>({
  jobName,
  jobFn,
  defaultJobOptions,
  jobSchedule,
  entities,
}: {
  // jobName - The user-defined job name in their .wasp file.
  jobName: Parameters<PgBoss['schedule']>[0]
  // jobFn - The user-defined async job callback function.
  jobFn: JobFn<Input, Output, Entities>
  // defaultJobOptions - pg-boss specific options for `boss.send()` applied to every `submit()` invocation,
  // which can overriden in that call.
  defaultJobOptions: PgBoss.Schedule['options']
  jobSchedule?: {
    cron: Parameters<PgBoss['schedule']>[1]
    args: Parameters<PgBoss['schedule']>[2]
    options: Parameters<PgBoss['schedule']>[3]
  }
  // Entities used by job, passed into callback context.
  entities: Entities
}) {
  // NOTE(shayne): We are not awaiting `pgBossStarted` here since we need to return an instance to the job
  // template, or else the NodeJS module bootstrapping process will block and fail as it would then depend
  // on a runtime resolution of the promise in `startServer()`.
  // Since `pgBossStarted` will resolve in the future, it may appear possible to send pg-boss
  // a job before we actually have registered the handler via `boss.work()`. However, even if NodeJS does
  // not execute this callback before any job `submit()` calls, this is not a problem since pg-boss allows you
  // to submit jobs even if there are no workers registered.
  // Once they are registered, they will just start on the first job in their queue.
  pgBossStarted.then(async (boss) => {
    // As a safety precaution against undefined behavior of registering different
    // functions for the same job name, remove all registered functions first.
    await boss.offWork(jobName)

    // This tells pg-boss to run given worker function when job with that name is submitted.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#work
    await boss.work<Input, Output>(
      jobName,
      pgBossCallbackWrapper<Input, Output, Entities>(jobFn, entities)
    )

    // If a job schedule is provided, we should schedule the recurring job.
    // If the schedule name already exists, it's updated to the provided cron expression, arguments, and options.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#scheduling
    if (jobSchedule) {
      const options: PgBoss.ScheduleOptions = {
        ...defaultJobOptions,
        ...jobSchedule.options,
      }
      await boss.schedule(
        jobName,
        jobSchedule.cron,
        jobSchedule.args || null,
        options
      )
    }
  })

  return new PgBossJob<Input, Output>(jobName, defaultJobOptions)
}

export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
> = (data: Input, context: { entities: Entities }) => Promise<Output>

/**
 * This is an interface repesenting a job that can be submitted to pg-boss.
 * It is not yet submitted until the caller invokes `submit()` on an instance.
 * The caller can make as many calls to `submit()` as they wish.
 */
class PgBossJob<
  Input extends JSONObject,
  Output extends JSONValue | void
> extends Job {
  public readonly defaultJobOptions: Parameters<PgBoss['send']>[2]
  public readonly startAfter: number | string | Date

  constructor(
    jobName: string,
    defaultJobOptions: Parameters<PgBoss['send']>[2],
    startAfter?: number | string | Date
  ) {
    super(jobName, PG_BOSS_EXECUTOR_NAME)
    this.defaultJobOptions = defaultJobOptions
    this.startAfter = startAfter
  }
  delay(startAfter: number | string | Date) {
    return new PgBossJob<Input, Output>(
      this.jobName,
      this.defaultJobOptions,
      startAfter
    )
  }
  async submit(jobArgs: Input, jobOptions: Parameters<PgBoss['send']>[2] = {}) {
    const boss = await pgBossStarted
    const jobId = await (boss.send as any)(this.jobName, jobArgs, {
      ...this.defaultJobOptions,
      ...(this.startAfter && { startAfter: this.startAfter }),
      ...jobOptions,
    })
    return new PgBossSubmittedJob<Input, Output>(boss, this, jobId)
  }
}

/**
 * A pg-boss specific SubmittedJob that adds additional pg-boss functionality.
 */
class PgBossSubmittedJob<
  Input extends JSONObject,
  Output extends JSONValue | void
> extends SubmittedJob {
  public readonly pgBoss: {
    readonly cancel: () => ReturnType<PgBoss['cancel']>
    readonly resume: () => ReturnType<PgBoss['resume']>
    readonly details: () => Promise<PgBossDetails<Input, Output> | null>
  }

  constructor(
    boss: PgBoss,
    job: PgBossJob<Input, Output>,
    jobId: SubmittedJob['jobId']
  ) {
    super(job, jobId)
    this.pgBoss = {
      cancel: () => boss.cancel(jobId),
      resume: () => boss.resume(jobId),
      // Coarcing here since pg-boss typings are not precise enough.
      details: () =>
        boss.getJobById(jobId) as Promise<PgBossDetails<Input, Output> | null>,
    }
  }
}

/**
 * Wraps the normal pg-boss callback function to inject entities, as well as extract
 * the `data` property so the arguments passed into the job are the exact same as those received.
 */
function pgBossCallbackWrapper<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
>(
  // jobFn - The user-defined async job callback function.
  jobFn: JobFn<Input, Output, Entities>,
  // Entities used by job, passed into callback context.
  entities: Entities
) {
  return (args: { data: Input }) => {
    const context = { entities }
    return jobFn(args.data, context)
  }
}

// Overrides the default pg-boss JobWithMetadata type to provide more
// type safety.
type PgBossDetails<
  Input extends JSONObject,
  Output extends JSONValue | void
> = Omit<PgBoss.JobWithMetadata<Input>, 'state' | 'output'> & {
  data: Input
} & (
    | {
        state: 'completed'
        output: JobOutputToMetadataOutput<Output>
      }
    | {
        state: 'failed'
        output: object
      }
    | {
        state: 'created' | 'retry' | 'active' | 'expired' | 'cancelled'
        output: null
      }
  )

// pg-boss wraps primitive values in an object with a `value` property.
// https://github.com/timgit/pg-boss/blob/master/src/manager.js#L526
type JobOutputToMetadataOutput<JobOutput> = JobOutput extends
  | null
  | undefined
  | void
  | Function
  ? null
  : JobOutput extends object
  ? JobOutput
  : { value: JobOutput }
