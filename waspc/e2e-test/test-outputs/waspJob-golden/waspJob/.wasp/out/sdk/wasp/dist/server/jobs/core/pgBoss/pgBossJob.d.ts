import PgBoss from 'pg-boss';
import { Job, SubmittedJob } from '../job.js';
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization';
import { PrismaDelegate } from 'wasp/server/_types';
import type { JobFn } from 'wasp/server/jobs/core/pgBoss';
export declare const PG_BOSS_EXECUTOR_NAME: unique symbol;
type JobSchedule = {
    cron: Parameters<PgBoss['schedule']>[1];
    args: Parameters<PgBoss['schedule']>[2];
    options: Parameters<PgBoss['schedule']>[3];
};
/**
 * Creates an instance of PgBossJob which contains all of the necessary
 * information to submit a job to pg-boss.
 */
export declare function createJobDefinition<Input extends JSONObject, Output extends JSONValue | void, Entities extends Partial<PrismaDelegate>>({ jobName, defaultJobOptions, jobSchedule, entities, }: {
    jobName: Parameters<PgBoss['schedule']>[0];
    defaultJobOptions: PgBoss.Schedule['options'];
    jobSchedule: JobSchedule | null;
    entities: Entities;
}): PgBossJob<Input, Output, Entities>;
/**
 * Uses the info about a job in PgBossJob to register a user defined job handler with pg-boss.
 * We expect this to be called once per job name. If called multiple times with the same name and different
 * functions, we will override the previous calls.
 */
export declare function registerJob<Input extends JSONObject, Output extends JSONValue | void, Entities extends Partial<PrismaDelegate>>({ job, jobFn }: {
    job: PgBossJob<Input, Output, Entities>;
    jobFn: JobFn<Input, Output, Entities>;
}): void;
/**
 * This is an interface repesenting a job that can be submitted to pg-boss.
 * It is not yet submitted until the caller invokes `submit()` on an instance.
 * The caller can make as many calls to `submit()` as they wish.
 */
declare class PgBossJob<Input extends JSONObject, Output extends JSONValue | void, Entities extends Partial<PrismaDelegate>> extends Job {
    readonly defaultJobOptions: Parameters<PgBoss['send']>[2];
    readonly startAfter: number | string | Date;
    readonly entities: Entities;
    readonly jobSchedule: JobSchedule | null;
    constructor(jobName: string, defaultJobOptions: Parameters<PgBoss['send']>[2], entities: Entities, jobSchedule: JobSchedule | null, startAfter?: number | string | Date);
    delay(startAfter: number | string | Date): PgBossJob<Input, Output, Entities>;
    submit(jobArgs: Input, jobOptions?: Parameters<PgBoss['send']>[2]): Promise<PgBossSubmittedJob<Input, Output, Entities>>;
}
/**
 * A pg-boss specific SubmittedJob that adds additional pg-boss functionality.
 */
declare class PgBossSubmittedJob<Input extends JSONObject, Output extends JSONValue | void, Entities extends Partial<PrismaDelegate>> extends SubmittedJob {
    readonly pgBoss: {
        readonly cancel: () => ReturnType<PgBoss['cancel']>;
        readonly resume: () => ReturnType<PgBoss['resume']>;
        readonly details: () => Promise<PgBossDetails<Input, Output> | null>;
    };
    constructor(boss: PgBoss, job: PgBossJob<Input, Output, Entities>, jobId: SubmittedJob['jobId']);
}
type PgBossDetails<Input extends JSONObject, Output extends JSONValue | void> = Omit<PgBoss.JobWithMetadata<Input>, 'state' | 'output'> & {
    data: Input;
} & ({
    state: 'completed';
    output: JobOutputToMetadataOutput<Output>;
} | {
    state: 'failed';
    output: object;
} | {
    state: 'created' | 'retry' | 'active' | 'expired' | 'cancelled';
    output: null;
});
type JobOutputToMetadataOutput<JobOutput> = JobOutput extends null | undefined | void | Function ? null : JobOutput extends object ? JobOutput : {
    value: JobOutput;
};
export {};
