import { prisma } from 'wasp/server';
import { createAuthenticatedOperation, } from '../wrappers.js';
import { foo as foo_ext } from 'wasp/src/server/queries/bar';
// PUBLIC API
export const mySpecialQuery = createAuthenticatedOperation(foo_ext, {
    User: prisma.user,
});
//# sourceMappingURL=index.js.map