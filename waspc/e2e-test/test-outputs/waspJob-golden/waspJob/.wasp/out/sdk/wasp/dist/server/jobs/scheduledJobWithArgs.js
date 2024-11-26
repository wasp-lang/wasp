import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {};
const jobSchedule = {
    cron: "0 * * * *",
    args: { "foo": "bar" },
    options: { "retryLimit": 2 },
};
// PUBLIC API
export const scheduledJobWithArgs = createJobDefinition({
    jobName: 'scheduledJobWithArgs',
    defaultJobOptions: {},
    jobSchedule,
    entities,
});
//# sourceMappingURL=scheduledJobWithArgs.js.map