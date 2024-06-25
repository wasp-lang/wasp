import { prisma } from 'wasp/server';
import { createAuthenticatedOperation, } from '../wrappers.js';
import { foo as foo_ext } from 'wasp/ext-src/server/actions/bar';
// PUBLIC API
export const mySpecialAction = createAuthenticatedOperation(foo_ext, {
    User: prisma.user,
});
//# sourceMappingURL=index.js.map