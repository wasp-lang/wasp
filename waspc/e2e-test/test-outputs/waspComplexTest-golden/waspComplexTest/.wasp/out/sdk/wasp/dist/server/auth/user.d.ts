import { type User, type Auth, type AuthIdentity } from '../../entities/index.js';
import { type PossibleProviderData } from '../../auth/utils.js';
import { type ProviderName } from '../_types/index.js';
import { Expand } from '../../universal/types.js';
export type AuthUser = AuthUserData & {
    getFirstProviderUserId: () => string | null;
};
export type AuthUserData = Omit<CompleteUserEntityWithAuth, 'auth'> & {
    identities: {
        google: Expand<UserFacingProviderData<'google'>> | null;
    };
};
type UserFacingProviderData<PN extends ProviderName> = {
    id: string;
} & Omit<PossibleProviderData[PN], 'hashedPassword'>;
export type CompleteUserEntityWithAuth = MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>;
export type CompleteAuthEntityWithIdentities = MakeAuthEntityWithIdentities<AuthIdentity>;
/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getUsername` and `getEmail`.
 */
export type UserEntityWithAuth = MakeUserEntityWithAuth<MakeAuthEntityWithIdentities<Pick<AuthIdentity, 'providerName' | 'providerUserId'>>>;
type MakeUserEntityWithAuth<AuthType> = User & {
    auth: AuthType | null;
};
type MakeAuthEntityWithIdentities<IdentityType> = Auth & {
    identities: IdentityType[];
};
export declare function createAuthUserData(user: CompleteUserEntityWithAuth): AuthUserData;
export {};
