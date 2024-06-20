import { type User, type Auth, type AuthIdentity } from '../../entities/index.js';
import { type PossibleProviderData } from '../../auth/utils.js';
import { type ProviderName } from '../_types/index.js';
import { Expand } from '../../universal/types.js';
export type AuthUser = AuthUserData & {
    getFirstProviderUserId: () => string | null;
};
export type AuthUserData = Omit<UserEntityWithAuth, 'auth'> & {
    identities: {
        google: Expand<UserFacingProviderData<'google'>> | null;
    };
};
type UserFacingProviderData<PN extends ProviderName> = {
    id: string;
} & Omit<PossibleProviderData[PN], 'hashedPassword'>;
export type UserEntityWithAuth = User & {
    auth: AuthEntityWithIdentities | null;
};
export type AuthEntityWithIdentities = Auth & {
    identities: AuthIdentity[];
};
/**
 * This Minimal* types are used for user facing helper functions like `getUsername` and `getEmail`.
 * By keeping the types minimal, we allow users to use these functions without having to send
 * the whole user object from the server to the client.
 */
export type MinimalUserEntityWithAuth = User & {
    auth: MinimalAuthEntityWithIdentities | null;
};
export type MinimalAuthEntityWithIdentities = Auth & {
    identities: MinimalAuthIdentityEntity[];
};
type MinimalAuthIdentityEntity = Pick<AuthIdentity, 'providerName' | 'providerUserId'>;
export declare function createAuthUserData(user: UserEntityWithAuth): AuthUserData;
export {};
