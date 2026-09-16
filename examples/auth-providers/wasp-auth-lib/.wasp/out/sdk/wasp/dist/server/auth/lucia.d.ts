import { Lucia } from "lucia";
import { type User } from "../../entities/index.js";
/**
 * We are using Lucia for session management.
 *
 * Some details:
 * 1. We are using the Prisma adapter for Lucia.
 * 2. We are not using cookies for session management. Instead, we are using
 *    the Authorization header to send the session token.
 * 3. Our `Session` entity is connected to the `Auth` entity.
 * 4. We are exposing the `userId` field from the `Auth` entity to
 *    make fetching the User easier.
 */
export declare const auth: Lucia<{
    providerId: string | null;
    providerSessionId: string | null;
}, {
    userId: User["id"] | null;
}>;
declare module "lucia" {
    interface Register {
        Lucia: typeof auth;
        DatabaseSessionAttributes: {
            providerId: string | null;
            providerSessionId: string | null;
        };
        DatabaseUserAttributes: {
            userId: User['id'] | null;
        };
    }
}
//# sourceMappingURL=lucia.d.ts.map