/**
 * Your app's [Jobs](https://wasp.sh/docs/features/jobs), which you can submit
 * from your server code.
 *
 * Wasp generates two exports per Job, named after the worker function you
 * pass to `job` in the Wasp Spec:
 *
 * - A value with the same name as the worker function (e.g. `mySpecialJob`),
 *   of type {@link PgBossJob}. Use it to submit the Job.
 * - A generic type named after the worker function in PascalCase (e.g.
 *   `MySpecialJob<Input, Output>`), which is a {@link JobFn} with the Job's
 *   Entities. Use it to type the worker function.
 *
 * @module
 */

import type {
  Job,
  JSONObject,
  JSONValue,
  SubmittedJob,
} from "@wasp/lib-sdk-core";

/**
 * The type of a Job's worker function: an `async` function that performs the
 * Job's work. The value it returns is the Job's result.
 *
 * Since Wasp executes Jobs on the server, the worker function's import path
 * must lead to a Node.js file.
 *
 * The type Wasp generates for each Job (e.g. `MySpecialJob` for
 * `mySpecialJob`) fills in `Entities`. You must pass it the other two type
 * arguments, `Input` and `Output`.
 *
 * @typeParam Input The type of the worker function's `args` argument.
 * @typeParam Output The type of the worker function's return value.
 * @typeParam Entities The type of `context.entities`.
 *
 * @param args The data you submit the Job with. For scheduled runs, it's the
 *   `args` in the Job's `schedule` field, or `null` if you didn't set them.
 * @param context The context object. Its `entities` field has the Prisma API
 *   for each Entity in the Job's `entities` field, keyed by the Entity's name
 *   (e.g. `context.entities.Task` exposes `prisma.task` from
 *   [Prisma's CRUD API](https://www.prisma.io/docs/orm/prisma-client/queries/crud)).
 *
 * @example
 * ```ts
 * import { type MySpecialJob } from "wasp/server/jobs";
 * import { type Task } from "wasp/entities";
 *
 * type Input = { name: string };
 * type Output = { tasks: Task[] };
 *
 * export const mySpecialJob: MySpecialJob<Input, Output> = async (
 *   { name },
 *   context,
 * ) => {
 *   console.log(`Hello ${name}!`);
 *   const tasks = await context.entities.Task.findMany({});
 *   return { tasks };
 * };
 * ```
 *
 * @placeholder
 */
export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities,
> = (args: Input, context: { entities: Entities }) => Promise<Output>;

/**
 * A Job you can submit to be executed by its executor, pg-boss.
 *
 * In the values Wasp generates (e.g. `mySpecialJob`), `Input` is `JSONObject`
 * and `Output` is `JSONValue | void`, so `submit` accepts any JSON object, not
 * only the `Input` type of the worker function.
 *
 * @typeParam Input The type of the data you submit the Job with.
 * @typeParam Output The type of the Job's result.
 * @typeParam Entities The Entities the Job uses.
 *
 * @placeholder
 */
export interface PgBossJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities,
> extends Job {
  /**
   * The Job's name, which Wasp derives from the worker function you pass to
   * `job` in the Wasp Spec.
   */
  readonly jobName: string;
  /** The name of the Job's executor (a `Symbol` for pg-boss). */
  readonly executorName: string | symbol;

  /**
   * Returns a copy of this Job that delays the invocation of the worker
   * function when you submit it.
   *
   * @param startAfter When to run the Job. One of:
   *
   * - A number: the number of seconds to delay.
   * - A string: a UTC ISO date string (ending in `Z`) to run at.
   * - A `Date`: the date to run at.
   *
   * @example
   * ```ts
   * import { mySpecialJob } from "wasp/server/jobs";
   *
   * const submittedJob = await mySpecialJob
   *   .delay(10)
   *   .submit({ name: "Johnny" }, { retryLimit: 2 });
   * ```
   */
  delay(startAfter: number | string | Date): PgBossJob<Input, Output, Entities>;

  /**
   * Submits the Job to be executed by pg-boss.
   *
   * @param jobArgs The JSON object the worker function receives as its `args`
   *   argument. Pass `{}` if the worker function takes no arguments. In
   *   JavaScript, you can also omit it, and the worker function receives
   *   `null`.
   * @param jobOptions pg-boss specific options for the submission. They
   *   override the ones in the Job's `performExecutorOptions.pgBoss` field in
   *   the Wasp Spec.
   *
   * @example
   * ```ts
   * import { mySpecialJob } from "wasp/server/jobs";
   *
   * const submittedJob = await mySpecialJob.submit({ name: "Johnny" });
   * ```
   */
  submit(
    jobArgs: Input,
    jobOptions?: PgBossSendOptions,
  ): Promise<PgBossSubmittedJob<Input>>;
}

/**
 * pg-boss's options for submitting a Job.
 *
 * This lists the most common ones. See all of them in
 * [pg-boss's `send` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options).
 */
type PgBossSendOptions = {
  /** Higher numbers have higher priority. */
  priority?: number;
  /** Delays the Job like {@link PgBossJob.delay}. (Default `0`) */
  startAfter?: number | string | Date;
  /**
   * Only allows one queued or active submission of this Job with this key at a
   * time. If there already is one, pg-boss doesn't create a new one, and the
   * submitted Job's `jobId` is `null`.
   */
  singletonKey?: string;
  /** How many times to retry the Job if it fails. */
  retryLimit?: number;
  /** How many seconds to wait between retries. */
  retryDelay?: number;
  /** Whether to use exponential backoff between retries. */
  retryBackoff?: boolean;
  /** How many seconds the Job can run before it expires. */
  expireInSeconds?: number;
  /** How many seconds to keep the Job in the queue before archiving it. */
  retentionSeconds?: number;
};

/**
 * The result of submitting a Job, which you can use to track it.
 */
interface PgBossSubmittedJob<Input extends JSONObject>
  extends Omit<SubmittedJob, "jobId"> {
  /**
   * The ID of the Job in pg-boss, or `null` if pg-boss didn't create the Job
   * because of a singleton option like `singletonKey`. In that case,
   * `pgBoss.cancel` and `pgBoss.resume` reject, and `pgBoss.details` resolves
   * to `null`.
   */
  readonly jobId: string | null;
  /**
   * The Job you submitted. It has the Job's `jobName` and `executorName`.
   */
  readonly job: Job;
  /**
   * pg-boss specific functions for the submitted Job.
   */
  readonly pgBoss: {
    /**
     * Gets pg-boss's details about the Job, or `null` if it doesn't exist.
     */
    readonly details: () => Promise<PgBossJobDetails<Input> | null>;
    /**
     * Attempts to cancel the Job. It rejects if {@link PgBossSubmittedJob.jobId}
     * is `null`.
     *
     * @see [pg-boss's `cancel` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#cancelid)
     */
    readonly cancel: () => Promise<PgBossUpdateResult>;
    /**
     * Attempts to resume a cancelled Job. It rejects if
     * {@link PgBossSubmittedJob.jobId} is `null`.
     *
     * @see [pg-boss's `resume` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#resumeid)
     */
    readonly resume: () => Promise<PgBossUpdateResult>;
  };
}

/**
 * pg-boss's result of attempting to cancel or resume a Job.
 */
type PgBossUpdateResult = {
  /** The IDs of the Jobs pg-boss tried to update: only the submitted Job's. */
  jobs: string[];
  /** How many Jobs pg-boss tried to update: always `1`. */
  requested: number;
  /**
   * How many Jobs pg-boss updated. It's `0` if pg-boss didn't change the Job
   * (e.g. when you try to cancel a Job that already finished).
   */
  updated: number;
};

/**
 * pg-boss's details about a submitted Job.
 *
 * This lists the most common fields. See all of them in
 * [pg-boss's `getJobById` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#getjobbyidid).
 */
type PgBossJobDetails<Input extends JSONObject> = {
  /** The ID of the Job in pg-boss. */
  id: string;
  /** The name of the Job. */
  name: string;
  /** The data the Job was submitted with. */
  data: Input;
  /** The Job's state. */
  state:
    | "created"
    | "retry"
    | "active"
    | "completed"
    | "expired"
    | "cancelled"
    | "failed";
  /**
   * The Job's result:
   *
   * - When `state` is `"completed"`, the worker function's return value.
   *   pg-boss wraps primitives and arrays in an object with a `value` property
   *   (e.g. `{ value: 42 }`), and stores `null` if it returned nothing.
   * - When `state` is `"failed"`, the error. A Job that failed and is waiting
   *   to be retried also has the error here.
   * - Otherwise, usually `null`.
   */
  output: object | null;
  /** How many times pg-boss has retried the Job. */
  retrycount: number;
  /** When the Job was created. */
  createdon: Date;
  /** When the Job started, or `null` if it hasn't started yet. */
  startedon: Date | null;
  /** When the Job completed, if it did. */
  completedon: Date | null;
};

export {};
