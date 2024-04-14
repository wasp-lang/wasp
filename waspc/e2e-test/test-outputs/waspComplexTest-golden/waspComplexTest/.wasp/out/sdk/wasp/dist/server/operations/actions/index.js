import { prisma } from 'wasp/server';
import { foo as foo_ext } from 'wasp/ext-src/server/actions/bar';
// PUBLIC API
export const mySpecialAction = async (args, context) => {
    return foo_ext(args, Object.assign(Object.assign({}, context), { entities: {
            User: prisma.user,
        } }));
};
//# sourceMappingURL=index.js.map