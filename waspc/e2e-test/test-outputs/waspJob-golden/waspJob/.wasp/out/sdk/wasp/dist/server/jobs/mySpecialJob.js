import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
const jobSchedule = null;
// PUBLIC API
export const mySpecialJob = createJobDefinition({
    jobName: 'mySpecialJob',
    defaultJobOptions: {},
    jobSchedule,
    entities,
});
//# sourceMappingURL=mySpecialJob.js.map