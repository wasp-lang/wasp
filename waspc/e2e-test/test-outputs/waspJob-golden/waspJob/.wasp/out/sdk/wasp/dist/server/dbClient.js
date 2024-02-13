// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
function createDbClient() {
    return null;
}
const dbClient = createDbClient();
// PUBLIC API
export default dbClient;
//# sourceMappingURL=dbClient.js.map