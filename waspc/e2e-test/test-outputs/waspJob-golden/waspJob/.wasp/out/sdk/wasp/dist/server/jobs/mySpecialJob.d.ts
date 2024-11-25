import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization';
import { type JobFn } from 'wasp/server/jobs/core/pgBoss';
declare const entities: {};
export type MySpecialJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>;
export declare const mySpecialJob: {
    readonly defaultJobOptions: Parameters<import("pg-boss")["send"]>[2];
    readonly startAfter: number | string | Date | undefined;
    readonly entities: {};
    readonly jobSchedule: {
        cron: Parameters<import("pg-boss")["schedule"]>[1];
        options: Parameters<import("pg-boss")["schedule"]>[3];
        args?: NonNullable<Parameters<import("pg-boss")["schedule"]>[2]>;
    } | null;
    delay(startAfter: number | string | Date): /*elided*/ any;
    submit(jobArgs: JSONObject, jobOptions?: Parameters<import("pg-boss")["send"]>[2]): Promise<{
        readonly pgBoss: {
            readonly cancel: () => ReturnType<import("pg-boss")["cancel"]>;
            readonly resume: () => ReturnType<import("pg-boss")["resume"]>;
            readonly details: () => Promise<(Omit<import("pg-boss").JobWithMetadata<JSONObject>, "state" | "output"> & ({
                data: JSONObject;
            } & ({
                state: "failed";
                output: object;
            } | {
                state: "created" | "retry" | "active" | "expired" | "cancelled";
                output: null;
            } | {
                state: "completed";
                output: import("wasp/server/_types/serialization").JSONArray | JSONObject | {
                    value: string;
                } | {
                    value: number;
                } | {
                    value: false;
                } | {
                    value: true;
                } | null;
            }))) | null>;
        };
        readonly job: import("./core/job").Job;
        readonly jobId: string;
    }>;
    readonly jobName: string;
    readonly executorName: string | symbol;
};
export {};
