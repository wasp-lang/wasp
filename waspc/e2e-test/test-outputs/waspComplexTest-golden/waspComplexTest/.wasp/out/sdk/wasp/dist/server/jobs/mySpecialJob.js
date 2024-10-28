import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
// PUBLIC API
export const mySpecialJob = createJobDefinition({
    jobName: 'mySpecialJob',
    defaultJobOptions: {},
    // TODO: jobSchdule template variable is a JSON string
    // and the "args" field is outputted as "null" but it should be "undefined"
    // when the value is not provided
    // @ts-ignore
    jobSchedule: null,
    entities,
});
//# sourceMappingURL=mySpecialJob.js.map