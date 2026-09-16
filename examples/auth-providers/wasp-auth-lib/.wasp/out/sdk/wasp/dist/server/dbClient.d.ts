import { PrismaClient as InternalPrismaClient } from '@prisma/client';
import type { FromRegister } from '../types/register';
declare function createDbClient(): InternalPrismaClient<{
    omit: {
        authIdentity: {
            providerSecrets: true;
        };
    };
}, never, import("@prisma/client/runtime/library.js").DefaultArgs>;
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;
export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', typeof createDbClient>;
declare const dbClient: PrismaClient;
export default dbClient;
//# sourceMappingURL=dbClient.d.ts.map