import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
const jobSchedule = null;
// PUBLIC API
export const simpleJob = createJobDefinition({
    jobName: 'simpleJob',
    defaultJobOptions: {},
    jobSchedule,
    entities,
});
//# sourceMappingURL=simpleJob.js.map