import { type User, type Auth, type AuthIdentity } from 'wasp/entities';
import { type ProviderName } from './types';
export declare function getEmail(user: UserEntityWithAuth): string | null;
export declare function getUsername(user: UserEntityWithAuth): string | null;
export declare function getFirstProviderUserId(user?: UserEntityWithAuth): string | null;
export declare function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): AuthIdentity | undefined;
export type AuthUser = ReturnType<typeof createAuthUser>;
export declare function createAuthUser(user: UserEntityWithAuth): {
    identities: {
        google: {
            id: string;
            data: import("./utils.js").OAuthProviderData;
        };
    };
    getFirstProviderUserId: () => string;
    _rawUser: UserEntityWithAuth;
    id: number;
    username: string;
    password: string;
};
type UserEntityWithAuth = User & {
    auth: AuthEntityWithIdentities;
};
type AuthEntityWithIdentities = Auth & {
    identities: AuthIdentity[];
};
export {};
