import { prisma } from '../../index';
import { createAuthenticatedOperation, } from '../wrappers';
import { getMyTasks as getMyTasks_ext } from 'virtual:wasp/user/operations';
// PUBLIC API
export const getMyTasks = createAuthenticatedOperation(() => getMyTasks_ext, {
    Task: prisma.task,
});
//# sourceMappingURL=index.js.map