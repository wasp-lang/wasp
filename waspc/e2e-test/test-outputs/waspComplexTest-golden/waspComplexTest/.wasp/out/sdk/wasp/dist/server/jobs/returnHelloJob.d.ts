import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization';
import { type JobFn } from 'wasp/server/jobs/core/pgBoss';
declare const entities: {
    User: import(".prisma/client").Prisma.UserDelegate<import(".prisma/client").Prisma.RejectOnNotFound | import(".prisma/client").Prisma.RejectPerOperation, import("@prisma/client/runtime").DefaultArgs>;
};
export type ReturnHelloJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>;
export declare const returnHelloJob: {
    readonly defaultJobOptions: import("pg-boss").SendOptions;
    readonly startAfter: string | number | Date;
    readonly entities: {
        User: import(".prisma/client").Prisma.UserDelegate<import(".prisma/client").Prisma.RejectOnNotFound | import(".prisma/client").Prisma.RejectPerOperation, import("@prisma/client/runtime").DefaultArgs>;
    };
    readonly jobSchedule: {
        cron: string;
        args: object;
        options: import("pg-boss").ScheduleOptions;
    };
    delay(startAfter: string | number | Date): any;
    submit(jobArgs: JSONObject, jobOptions?: import("pg-boss").SendOptions): Promise<{
        readonly pgBoss: {
            readonly cancel: () => Promise<void>;
            readonly resume: () => Promise<void>;
            readonly details: () => Promise<Omit<import("pg-boss").JobWithMetadata<JSONObject>, "output" | "state"> & {
                data: JSONObject;
            } & ({
                state: "failed";
                output: object;
            } | {
                state: "retry" | "created" | "active" | "expired" | "cancelled";
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
                };
            })>;
        };
        readonly job: import("./core/job").Job;
        readonly jobId: string;
    }>;
    readonly jobName: string;
    readonly executorName: string | symbol;
};
export {};
