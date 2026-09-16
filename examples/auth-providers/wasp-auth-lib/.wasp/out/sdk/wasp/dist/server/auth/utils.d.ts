import { HttpError } from '../index.js';
import { type User, type Auth } from '../../entities/index.js';
import { Prisma } from '@prisma/client';
import { type UserSignupFields } from '../../auth/providers/types.js';
export { createProviderId, normalizeProviderUserId, parseProviderData, parseProviderSecrets, type ProviderId, type ProviderName, type PossibleProviderData, type PossibleProviderSecrets, type EmailProviderData, type EmailProviderSecrets, type UsernameProviderData, type UsernameProviderSecrets, type OAuthProviderData, type OAuthProviderSecrets, } from '../../auth/providerData.js';
export declare const contextWithUserEntity: {
    entities: {
        User: Prisma.UserDelegate<import("@prisma/client/runtime/library.js").DefaultArgs, {
            omit: {
                authIdentity: {
                    providerSecrets: true;
                };
            };
        }>;
    };
};
export declare const authConfig: {
    failureRedirectPath: string;
    successRedirectPath: string;
};
export type FindAuthWithUserResult = Auth & {
    user: User;
};
export declare function findAuthWithUserBy(where: Prisma.AuthWhereInput): Promise<FindAuthWithUserResult | null>;
export declare function doFakeWork(): Promise<unknown>;
export declare function rethrowPossibleAuthError(e: unknown): void;
export declare function validateAndGetUserFields(data: {
    [key: string]: unknown;
}, userSignupFields?: UserSignupFields): Promise<Record<string, any>>;
export declare function createInvalidCredentialsError(message?: string): HttpError;
//# sourceMappingURL=utils.d.ts.map