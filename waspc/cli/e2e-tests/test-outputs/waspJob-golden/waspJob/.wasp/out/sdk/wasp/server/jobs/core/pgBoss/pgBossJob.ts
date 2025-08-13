import PgBoss from 'pg-boss'
import { pgBossStarted } from './pgBoss.js'
import { Job, SubmittedJob } from '../job.js'
import type { JSONValue, JSONObject } from 'wasp/core/serialization'
import { PrismaDelegate } from 'wasp/server/_types'
import type { JobFn } from 'wasp/server/jobs/core/pgBoss'

export const PG_BOSS_EXECUTOR_NAME = Symbol('PgBoss')

type JobSchedule = {
  cron: Parameters<PgBoss['schedule']>[1]
  options: Parameters<PgBoss['schedule']>[3]
  args?: NonNullable<Parameters<PgBoss['schedule']>[2]>
}

// PRIVATE API
/**
 * Creates an instance of PgBossJob which contains all of the necessary
 * information to submit a job to pg-boss.
 */
export function createJobDefinition<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
>({
  jobName,
  defaultJobOptions,
  jobSchedule,
  entities,
}: {
  // jobName - The user-defined job name in their .wasp file.
  jobName: PgBossJob<Input, Output, Entities>['jobName']
  // defaultJobOptions - pg-boss specific options for `boss.send()` applied to every `submit()` invocation,
  // which can overriden in that call.
  defaultJobOptions: PgBossJob<Input, Output, Entities>['defaultJobOptions']
  jobSchedule: PgBossJob<Input, Output, Entities>['jobSchedule']
  // Entities used by job, passed into callback context.
  entities: Entities
}) {
  return new PgBossJob<Input, Output, Entities>(
    jobName,
    defaultJobOptions,
    entities,
    jobSchedule,
  )
}

// PRIVATE API
/**
 * Uses the info about a job in PgBossJob to register a user defined job handler with pg-boss.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 */
export function registerJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
>({ job, jobFn }: {
  job: PgBossJob<Input, Output, Entities>,
  jobFn: JobFn<Input, Output, Entities>,
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
    await boss.offWork(job.jobName)

    // This tells pg-boss to run given worker function when job with that name is submitted.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#work
    await boss.work<Input, Output>(
      job.jobName,
      pgBossCallbackWrapper<Input, Output, Entities>(jobFn, job.entities)
    )

    // If a job schedule is provided, we should schedule the recurring job.
    // If the schedule name already exists, it's updated to the provided cron expression, arguments, and options.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#scheduling
    if (job.jobSchedule) {
      const options: PgBoss.ScheduleOptions = {
        ...job.defaultJobOptions,
        ...job.jobSchedule.options,
      }
      await boss.schedule(
        job.jobName,
        job.jobSchedule.cron,
        job.jobSchedule.args,
        options
      )
    }
  })
}

/**
 * This is an interface repesenting a job that can be submitted to pg-boss.
 * It is not yet submitted until the caller invokes `submit()` on an instance.
 * The caller can make as many calls to `submit()` as they wish.
 */
export class PgBossJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
> extends Job {
  public readonly defaultJobOptions: Parameters<PgBoss['send']>[2]
  public readonly startAfter: number | string | Date | undefined
  public readonly entities: Entities
  public readonly jobSchedule: JobSchedule | null

  constructor(
    jobName: string,
    defaultJobOptions: Parameters<PgBoss['send']>[2],
    entities: Entities,
    jobSchedule: JobSchedule | null,
    startAfter?: number | string | Date
  ) {
    super(jobName, PG_BOSS_EXECUTOR_NAME)
    this.defaultJobOptions = defaultJobOptions
    this.entities = entities
    this.jobSchedule = jobSchedule
    this.startAfter = startAfter
  }
  delay(startAfter: number | string | Date) {
    return new PgBossJob<Input, Output, Entities>(
      this.jobName,
      this.defaultJobOptions,
      this.entities,
      this.jobSchedule,
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
    return new PgBossSubmittedJob<Input, Output, Entities>(boss, this, jobId)
  }
}

/**
 * A pg-boss specific SubmittedJob that adds additional pg-boss functionality.
 */
class PgBossSubmittedJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities extends Partial<PrismaDelegate>
> extends SubmittedJob {
  public readonly pgBoss: {
    readonly cancel: () => ReturnType<PgBoss['cancel']>
    readonly resume: () => ReturnType<PgBoss['resume']>
    readonly details: () => Promise<PgBossDetails<Input, Output> | null>
  }

  constructor(
    boss: PgBoss,
    job: PgBossJob<Input, Output, Entities>,
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
