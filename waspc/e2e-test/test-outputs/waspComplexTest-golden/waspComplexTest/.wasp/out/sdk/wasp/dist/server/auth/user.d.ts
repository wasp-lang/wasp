import { type User, type Auth, type AuthIdentity } from '../../entities/index.js';
import { type PossibleProviderData } from '../../auth/utils.js';
import { type ProviderName } from '../_types/index.js';
import { Expand } from '../../universal/types.js';
/**
 * Ideally, we'd do something like this:
 * ```
 * export type AuthUser = ReturnType<typeof createAuthUser>
 * ```
 * to get the benefits of the createAuthUser and the AuthUser type being in sync.
 *
 * But since we are not using strict mode, the inferred return type of createAuthUser
 * is not correct. So we have to define the AuthUser type manually.
 *
 * TODO: Change this once/if we switch to strict mode. https://github.com/wasp-lang/wasp/issues/1938
 */
export type AuthUserData = Omit<UserEntityWithAuth, 'auth'> & {
    identities: {
        google: Expand<UserFacingProviderData<'google'>> | null;
    };
};
export type AuthUser = AuthUserData & {
    getFirstProviderUserId: () => string | null;
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
export declare function createAuthUser(user: UserEntityWithAuth): AuthUserData;
export {};
