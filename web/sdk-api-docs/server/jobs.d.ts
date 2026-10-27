/**
 * Your app's [Jobs](https://wasp.sh/docs/features/jobs), which you can submit
 * from your server code.
 *
 * Wasp generates two exports per Job, named after the worker function you
 * pass to `job` in the app spec:
 *
 * - A value with the same name as the worker function (e.g. `mySpecialJob`),
 *   of type {@link PgBossJob}. Use it to submit the Job.
 * - A generic type named after the worker function in PascalCase (e.g.
 *   `MySpecialJob<Input, Output>`), which is a {@link JobFn} with the Job's
 *   Entities. Use it to type the worker function.
 *
 * @example
 * ```ts
 * import { mySpecialJob } from "wasp/server/jobs";
 *
 * const submittedJob = await mySpecialJob.submit({ name: "Johnny" });
 *
 * // Or, if you'd prefer it to execute in the future, add a `.delay()`.
 * await mySpecialJob.delay(10).submit({ name: "Johnny" });
 * ```
 *
 * @see [Jobs](https://wasp.sh/docs/features/jobs)
 *
 * @module
 */

import type {
  Job,
  JSONObject,
  JSONValue,
  SubmittedJob,
} from "@wasp.sh/lib-sdk-core";

/**
 * The type of a Job's worker function: an `async` function that performs the
 * Job's work. The value it returns is the Job's result.
 *
 * Since Wasp executes Jobs on the server, the worker function's import path
 * must lead to a Node.js file.
 *
 * Wasp generates one generic type per Job, named after the worker function in
 * PascalCase (e.g. `MySpecialJob` for `mySpecialJob`), which is this type with
 * the `Entities` of the Job. It takes the `Input` and `Output` type arguments.
 *
 * @typeParam Input The type of the worker function's `args` argument.
 * @typeParam Output The type of the worker function's return value.
 * @typeParam Entities The type of `context.entities`: an object with the
 *   Prisma API for each Entity in the Job's `entities` field (e.g.
 *   `{ Task: ... }` for `entities: ["Task"]`). Wasp fills this in for you in
 *   the generated type.
 *
 * @param args The data passed to the Job when it's submitted.
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
 * @see [Jobs](https://wasp.sh/docs/features/jobs)
 */
export type JobFn<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities,
> = (args: Input, context: { entities: Entities }) => Promise<Output>;

/**
 * A Job you can submit to be executed by its executor, pg-boss.
 *
 * Wasp generates one value of this type per Job, named after the worker
 * function you pass to `job` in the app spec (e.g. `mySpecialJob`).
 *
 * In the generated values, `Input` is `JSONObject` and `Output` is
 * `JSONValue | void`, so `submit` accepts any JSON object, not only the
 * `Input` type of the worker function.
 *
 * @typeParam Input The type of the data you submit the Job with.
 * @typeParam Output The type of the Job's result.
 * @typeParam Entities The Entities the Job uses.
 */
export interface PgBossJob<
  Input extends JSONObject,
  Output extends JSONValue | void,
  Entities,
> extends Job {
  /**
   * The Job's name, which Wasp derives from the worker function you pass to
   * `job` in the app spec.
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
   * - A number: the number of seconds to delay. (Default `0`)
   * - A string: an ISO date string to run at.
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
   *   argument.
   * @param jobOptions pg-boss specific options for the submission. They
   *   override the ones in the Job's `performExecutorOptions.pgBoss` field in
   *   the app spec.
   * @returns A {@link PgBossSubmittedJob} you can use to track the Job.
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
  /** Delays the Job like {@link PgBossJob.delay}. */
  startAfter?: number | string | Date;
  /** Only allows one Job with this key in the queue at a time. */
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
interface PgBossSubmittedJob<Input extends JSONObject> extends SubmittedJob {
  /** The ID of the Job in pg-boss. */
  readonly jobId: string;
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
     *
     * @see [pg-boss's `getJobById` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#getjobbyidid)
     */
    readonly details: () => Promise<PgBossJobDetails<Input> | null>;
    /**
     * Attempts to cancel the Job.
     *
     * @see [pg-boss's `cancel` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#cancelid)
     */
    readonly cancel: () => Promise<void>;
    /**
     * Attempts to resume the Job, if it was cancelled.
     *
     * @see [pg-boss's `resume` docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#resumeid)
     */
    readonly resume: () => Promise<void>;
  };
}

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
   *   pg-boss wraps primitive values in an object with a `value` property
   *   (e.g. `{ value: 42 }`), and stores `null` if it returned nothing.
   * - When `state` is `"failed"`, the error.
   * - Otherwise, `null`.
   */
  output: object | null;
  /** How many times pg-boss has retried the Job. */
  retrycount: number;
  /** When the Job was created. */
  createdon: Date;
  /** When the Job started. */
  startedon: Date;
  /** When the Job completed, if it did. */
  completedon: Date | null;
};

export {};
