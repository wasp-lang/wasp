import { prisma } from 'wasp/server';
import { foo as foo__userDefined } from 'wasp/ext-src/server/queries/bar.js';
// PUBLIC API
export const mySpecialQuery = async (args, context) => {
    return foo__userDefined(args, Object.assign(Object.assign({}, context), { entities: {
            User: prisma.user,
        } }));
};
//# sourceMappingURL=index.js.map