import { prisma } from 'wasp/server';
import { createJobDefinition } from 'wasp/server/jobs/core/pgBoss';
const entities = {
    User: prisma.user,
};
// PUBLIC API
export const returnHelloJob = createJobDefinition({
    jobName: 'returnHelloJob',
    defaultJobOptions: {},
    // TODO: jobSchdule template variable is a JSON string
    // and the "args" field is outputted as "null" but it should be "undefined"
    // when the value is not provided
    // @ts-ignore
    jobSchedule: null,
    entities,
});
//# sourceMappingURL=returnHelloJob.js.map