export type PrismaClient = null;

// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient: PrismaClient = null;

// PUBLIC API
export default dbClient;
