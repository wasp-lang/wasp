import { pgBossStarted } from "./pgBoss.js";
import { Job, createJob as jobConstructor } from "../Job.js";
import { SubmittedJob, createSubmittedJob } from "../SubmittedJob.js";
import PgBoss from "pg-boss";

export const PG_BOSS_EXECUTOR_NAME = Symbol("PgBoss");

/**
 * A pg-boss specific SubmittedJob that adds additional pg-boss functionality.
 */
interface PgBossSubmittedJob<
  Input extends object,
  Output extends object,
  Entities
> extends SubmittedJob<Input, Output, Entities> {
  pgBoss: {
    cancel: () => ReturnType<PgBoss["cancel"]>;
    resume: () => ReturnType<PgBoss["resume"]>;
    details: () => Promise<PbBossDetails<Input, Output> | null>;
  };
}

interface PbBossDetails<Input extends object, Output extends object>
  extends PgBoss.JobWithMetadata {
  data: Input;
  output: Output;
}

export function createPgBossSubmittedJob<
  Input extends object,
  Output extends object,
  Entities
>(
  boss: PgBoss,
  job: Job<Input, Output, Entities>,
  jobId: SubmittedJob<Input, Output, Entities>["jobId"]
) {
  return {
    ...createSubmittedJob(job, jobId),
    pgBoss: {
      cancel: () => boss.cancel(jobId),
      resume: () => boss.resume(jobId),
      // TODO: figure this out
      details: () => boss.getJobById(jobId) as any,
    },
  } satisfies PgBossSubmittedJob<Input, Output, Entities>;
}

/**
 * This is an interface repesenting a job that can be submitted to pg-boss.
 * It is not yet submitted until the caller invokes `submit()` on an instance.
 * The caller can make as many calls to `submit()` as they wish.
 */
interface PgBossJob<Input extends object, Output extends object, Entities>
  extends Job<Input, Output, Entities> {
  defaultJobOptions: Parameters<PgBoss["send"]>[2];
  startAfter: number | string | Date;
  delay: (
    startAfter: number | string | Date
  ) => PgBossJob<Input, Output, Entities>;
  submit: (
    jobArgs: Input,
    jobOptions?: Parameters<PgBoss["send"]>[2]
  ) => Promise<PgBossSubmittedJob<Input, Output, Entities>>;
}

export function createPgBossJob<
  Input extends object,
  Output extends object,
  Entities
>(
  // jobName - The name of the Job. This is what will show up in the pg-boss DB tables.
  jobName: string,
  // defaultJobOptions - Default options passed to `boss.send()`.
  defaultJobOptions: Parameters<PgBoss["send"]>[2],
  // startAfter - Defers job execution
  // * - number: Seconds to delay starting the job [Default: 0]
  // * - string: Start after a UTC Date time string in 8601 format
  // * - Date: Start after a Date object
  startAfter: number | string | Date = undefined
): PgBossJob<Input, Output, Entities> {
  return {
    ...jobConstructor(jobName, PG_BOSS_EXECUTOR_NAME),
    defaultJobOptions,
    startAfter,
    delay(startAfter: number | string | Date) {
      return createPgBossJob(jobName, defaultJobOptions, startAfter);
    },
    async submit(
      // jobArgs - The job arguments supplied by the user for their perform callback.
      jobArgs: Input,
      // jobOptions - pg-boss specific options for `boss.send()`, which can override
      // their defaultJobOptions.
      jobOptions: Parameters<PgBoss["send"]>[2]
    ) {
      const boss = await pgBossStarted;
      const jobId = await boss.send(jobName, jobArgs, {
        ...defaultJobOptions,
        ...(startAfter && { startAfter }),
        ...jobOptions,
      });
      return createPgBossSubmittedJob(boss, this, jobId);
    },
  };
}

/**
 * Creates an instance of PgBossJob and initializes the PgBoss executor by registering this job function.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 */
export function createJob<
  Input extends object,
  Output extends object,
  Entities
>({
  jobName,
  jobFn,
  defaultJobOptions,
  jobSchedule,
  entities,
}: {
  // jobName - The user-defined job name in their .wasp file.
  jobName: Parameters<PgBoss["schedule"]>[0];
  // jobFn - The user-defined async job callback function.
  jobFn: Job<Input, Output, Entities>["jobFn"];
  // defaultJobOptions - pg-boss specific options for `boss.send()` applied to every `submit()` invocation,
  // which can overriden in that call.
  defaultJobOptions: PgBoss.Schedule["options"];
  jobSchedule?: {
    cron: Parameters<PgBoss["schedule"]>[1];
    args: Parameters<PgBoss["schedule"]>[2];
    options: Parameters<PgBoss["schedule"]>[3];
  };
  // Entities used by job, passed into callback context.
  entities: Entities;
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
    await boss.offWork(jobName);

    // This tells pg-boss to run given worker function when job with that name is submitted.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#work
    // TODO: type this properly
    await boss.work<Input, Output>(
      jobName,
      pgBossCallbackWrapper<Input, Output, Entities>(jobFn, entities)
    );

    // If a job schedule is provided, we should schedule the recurring job.
    // If the schedule name already exists, it's updated to the provided cron expression, arguments, and options.
    // Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#scheduling
    if (jobSchedule) {
      const options: PgBoss.ScheduleOptions = {
        ...defaultJobOptions,
        ...jobSchedule.options,
      };
      await boss.schedule(
        jobName,
        jobSchedule.cron,
        jobSchedule.args || null,
        options
      );
    }
  });

  return createPgBossJob<Input, Output, Entities>(jobName, defaultJobOptions);
}

/**
 * Wraps the normal pg-boss callback function to inject entities, as well as extract
 * the `data` property so the arguments passed into the job are the exact same as those received.
 */
function pgBossCallbackWrapper<Input, Output extends object, Entities>(
  // jobFn - The user-defined async job callback function.
  jobFn: Job<Input, Output, Entities>["jobFn"],
  // TODO: figure this out
  // Entities used by job, passed into callback context.
  entities: Entities
) {
  return (args: { data: Input }) => {
    const context = { entities };
    return jobFn(args.data, context);
  };
}
