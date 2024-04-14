import { prisma } from 'wasp/server';
import { foo as foo_ext } from 'wasp/ext-src/server/queries/bar';
// PUBLIC API
export const mySpecialQuery = async (args, context) => {
    return foo_ext(args, Object.assign(Object.assign({}, context), { entities: {
            User: prisma.user,
        } }));
};
//# sourceMappingURL=index.js.map