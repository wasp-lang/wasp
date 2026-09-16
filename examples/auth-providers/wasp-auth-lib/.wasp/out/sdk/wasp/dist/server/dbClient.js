import { PrismaClient as InternalPrismaClient } from '@prisma/client';
function createDbClient() {
    return new InternalPrismaClient({
        // The auth identity's secret material (password hashes, ...) never leaves
        // this column unless auth internals opt back in per query -- so it cannot
        // end up in an operation result or a log by accident.
        omit: {
            authIdentity: {
                providerSecrets: true,
            },
        },
    });
}
const dbClient = createDbClient();
// PUBLIC API
export default dbClient;
//# sourceMappingURL=dbClient.js.map