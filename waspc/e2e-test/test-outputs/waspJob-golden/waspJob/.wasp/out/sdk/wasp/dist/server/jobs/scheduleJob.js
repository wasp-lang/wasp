import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
const jobSchedule = {
    cron: "0 * * * *",
    options: { "retryLimit": 2 },
};
// PUBLIC API
export const scheduleJob = createJobDefinition({
    jobName: 'scheduleJob',
    defaultJobOptions: {},
    jobSchedule,
    entities,
});
//# sourceMappingURL=scheduleJob.js.map