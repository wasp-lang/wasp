import { type User, type Auth, type AuthIdentity } from 'wasp/entities';
import { Prisma } from '@prisma/client';
import { type UserSignupFields, type PossibleUserFields } from './providers/types.js';
export type EmailProviderData = {
    hashedPassword: string;
    isEmailVerified: boolean;
    emailVerificationSentAt: string | null;
    passwordResetSentAt: string | null;
};
export type UsernameProviderData = {
    hashedPassword: string;
};
export type OAuthProviderData = {};
/**
 * This type is used for type-level programming e.g. to enumerate
 * all possible provider data types.
 *
 * The keys of this type are the names of the providers and the values
 * are the types of the provider data.
 */
export type PossibleProviderData = {
    email: EmailProviderData;
    username: UsernameProviderData;
    google: OAuthProviderData;
    github: OAuthProviderData;
};
export type ProviderName = keyof PossibleProviderData;
export declare const contextWithUserEntity: {
    entities: {
        User: Prisma.UserDelegate<Prisma.RejectOnNotFound | Prisma.RejectPerOperation, import("@prisma/client/runtime/index.js").DefaultArgs>;
    };
};
export declare const authConfig: {
    failureRedirectPath: string;
    successRedirectPath: string;
};
/**
 * ProviderId uniquely identifies an auth identity e.g.
 * "email" provider with user id "test@test.com" or
 * "google" provider with user id "1234567890".
 *
 * We use this type to avoid passing the providerName and providerUserId
 * separately. Also, we can normalize the providerUserId to make sure it's
 * consistent across different DB operations.
 */
export type ProviderId = {
    providerName: ProviderName;
    providerUserId: string;
};
export declare function createProviderId(providerName: ProviderName, providerUserId: string): ProviderId;
export declare function findAuthIdentity(providerId: ProviderId): Promise<AuthIdentity | null>;
/**
 * Updates the provider data for the given auth identity.
 *
 * This function performs data sanitization and serialization.
 * Sanitization is done by hashing the password, so this function
 * expects the password received in the `providerDataUpdates`
 * **not to be hashed**.
 */
export declare function updateAuthIdentityProviderData<PN extends ProviderName>(providerId: ProviderId, existingProviderData: PossibleProviderData[PN], providerDataUpdates: Partial<PossibleProviderData[PN]>): Promise<AuthIdentity>;
type FindAuthWithUserResult = Auth & {
    user: User;
};
export declare function findAuthWithUserBy(where: Prisma.AuthWhereInput): Promise<FindAuthWithUserResult>;
export declare function createUser(providerId: ProviderId, serializedProviderData?: string, userFields?: PossibleUserFields): Promise<User & {
    auth: Auth;
}>;
export declare function deleteUserByAuthId(authId: string): Promise<{
    count: number;
}>;
export declare function verifyToken<T = unknown>(token: string): Promise<T>;
export declare function doFakeWork(): Promise<unknown>;
export declare function rethrowPossibleAuthError(e: unknown): void;
export declare function validateAndGetUserFields(data: {
    [key: string]: unknown;
}, userSignupFields?: UserSignupFields): Promise<Record<string, any>>;
export declare function deserializeAndSanitizeProviderData<PN extends ProviderName>(providerData: string, { shouldRemovePasswordField }?: {
    shouldRemovePasswordField?: boolean;
}): PossibleProviderData[PN];
export declare function sanitizeAndSerializeProviderData<PN extends ProviderName>(providerData: PossibleProviderData[PN]): Promise<string>;
export declare function throwInvalidCredentialsError(message?: string): void;
export {};
