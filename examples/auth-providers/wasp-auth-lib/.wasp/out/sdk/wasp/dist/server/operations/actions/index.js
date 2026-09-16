import { prisma } from '../../index';
import { createAuthenticatedOperation, } from '../wrappers';
import { createTask as createTask_ext } from 'virtual:wasp/user/operations';
// PUBLIC API
export const createTask = createAuthenticatedOperation(() => createTask_ext, {
    Task: prisma.task,
});
//# sourceMappingURL=index.js.map