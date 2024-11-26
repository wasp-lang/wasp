import { prisma } from 'wasp/server';
import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {
    User: prisma.user,
};
const jobSchedule = null;
// PUBLIC API
export const returnHelloJob = createJobDefinition({
    jobName: 'returnHelloJob',
    defaultJobOptions: {},
    jobSchedule,
    entities,
});
//# sourceMappingURL=returnHelloJob.js.map