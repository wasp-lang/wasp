import { PrismaClient as InternalPrismaClient } from '@prisma/client';
declare const dbClient: InternalPrismaClient<import(".prisma/client").Prisma.PrismaClientOptions, never, import("@prisma/client/runtime/library.js").DefaultArgs>;
export type PrismaClient = typeof dbClient;
export default dbClient;
//# sourceMappingURL=dbClient.d.ts.map