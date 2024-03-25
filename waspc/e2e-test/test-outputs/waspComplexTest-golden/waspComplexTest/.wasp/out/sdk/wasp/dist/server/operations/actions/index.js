import { prisma } from 'wasp/server';
import { foo as __userDefinedFoo } from 'wasp/ext-src/server/actions/bar.js';
// PUBLIC API
export const mySpecialAction = async (args, context) => {
    return __userDefinedFoo(args, Object.assign(Object.assign({}, context), { entities: {
            User: prisma.user,
        } }));
};
//# sourceMappingURL=index.js.map