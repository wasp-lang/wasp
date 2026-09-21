import { type IdentityStore } from "@wasp.sh/auth-contract";
import { TimeSpan } from "@wasp.sh/lib-auth/node";
import { HttpError } from "./http.js";
import type { UserSignupFields, WaspAuthRuntime } from "./types.js";
export declare function doFakeWork(): Promise<unknown>;
export declare function createInvalidCredentialsError(message?: string): HttpError;
/** The same error translation the in-tree flows applied, duck-typed on Prisma's error names. */
export declare function rethrowPossibleAuthError(e: unknown): never;
export declare function validateAndGetUserFields(data: Record<string, unknown>, userSignupFields?: UserSignupFields): Promise<Record<string, unknown>>;
/** The app's JWT helpers, on the secret Wasp hands this provider through its env. */
export declare function makeJwt(runtime: WaspAuthRuntime): {
    createJWT: (data: Record<any, any>, options: {
        headers?: Record<any, any>;
        expiresIn?: TimeSpan;
        issuer?: string;
        subject?: string;
        audiences?: string[];
        notBefore?: Date;
        includeIssuedTimestamp?: boolean;
        jwtId?: string;
    } | undefined) => Promise<string>;
    validateJWT: <T>(token: string) => Promise<T>;
};
export { TimeSpan };
/**
 * The password hash of an identity, or null when it has none.
 *
 * Reads it from the identity's secrets. An identity written before Wasp
 * split secrets from data keeps its hash in `data` (the `providerData`
 * column, which the Prisma client does not hide from app code). Such a hash
 * is moved to the secrets on first read, so an upgraded app needs no data
 * migration and the hash leaves the readable column.
 */
export declare function getHashedPassword(identities: IdentityStore, providerUserId: string): Promise<string | null>;
