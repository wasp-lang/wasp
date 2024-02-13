import { prisma } from 'wasp/server';
import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {
    User: prisma.user,
};
// PUBLIC API
export const returnHelloJob = createJobDefinition({
    jobName: 'returnHelloJob',
    defaultJobOptions: {},
    jobSchedule: null,
    entities,
});
//# sourceMappingURL=returnHelloJob.js.map