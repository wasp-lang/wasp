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
