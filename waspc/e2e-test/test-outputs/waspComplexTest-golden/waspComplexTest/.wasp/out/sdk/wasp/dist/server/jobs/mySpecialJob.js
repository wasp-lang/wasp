import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
// PUBLIC API
export const mySpecialJob = createJobDefinition({
    jobName: 'mySpecialJob',
    defaultJobOptions: {},
    jobSchedule: null,
    entities,
});
//# sourceMappingURL=mySpecialJob.js.map