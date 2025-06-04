import { PrismaClient as InternalPrismaClient } from '@prisma/client';
function createDbClient() {
    return new InternalPrismaClient();
}
const dbClient = createDbClient();
// PUBLIC API
export default dbClient;
//# sourceMappingURL=dbClient.js.map