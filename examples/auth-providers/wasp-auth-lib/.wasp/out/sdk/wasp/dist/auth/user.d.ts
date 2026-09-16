import { type User, type Auth, type AuthIdentity } from '../entities/index.js';
import { type AuthProviderId } from './provider.js';
export declare function getEmail(user: UserEntityWithAuth): string | null;
export declare function getUsername(user: UserEntityWithAuth): string | null;
export declare function getFirstProviderUserId(user?: UserEntityWithAuth): string | null;
export type AuthUser = AuthUserData & {
    getFirstProviderUserId: () => string | null;
};
export type AuthUserData = Omit<CompleteUserEntityWithAuth, 'auth'> & {
    /**
     * Id of the auth provider that minted the current session -- i.e. how this
     * user logged in this time ('wasp', 'external:clerk', ...). A session is
     * always minted by exactly one provider, so this is a single compile-checked
     * literal, pinned when the session was created and never re-derived.
     */
    sessionProviderId: AuthProviderId;
    identities: {};
};
export type CompleteUserEntityWithAuth = MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>;
export type CompleteAuthEntityWithIdentities = MakeAuthEntityWithIdentities<Omit<AuthIdentity, 'providerSecrets'>>;
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
export declare function makeAuthUserIfPossible(user: null): null;
export declare function makeAuthUserIfPossible(user: AuthUserData): AuthUser;
export declare function makeAuthUserIfPossible(user: AuthUserData | null): AuthUser | null;
export declare function createAuthUserData(user: CompleteUserEntityWithAuth, sessionProviderId: string): AuthUserData;
export {};
//# sourceMappingURL=user.d.ts.map