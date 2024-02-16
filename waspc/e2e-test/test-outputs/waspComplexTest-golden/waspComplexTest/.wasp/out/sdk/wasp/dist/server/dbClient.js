import Prisma from '@prisma/client';
function createDbClient() {
    return new Prisma.PrismaClient();
}
const dbClient = createDbClient();
// PUBLIC API
export default dbClient;
//# sourceMappingURL=dbClient.js.map