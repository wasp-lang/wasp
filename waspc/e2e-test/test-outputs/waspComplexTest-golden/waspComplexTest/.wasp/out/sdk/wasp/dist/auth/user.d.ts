import type { AuthUser, ProviderName, DeserializedAuthIdentity } from './types';
export declare function getEmail(user: AuthUser): string | null;
export declare function getUsername(user: AuthUser): string | null;
export declare function getFirstProviderUserId(user?: AuthUser): string | null;
export declare function findUserIdentity(user: AuthUser, providerName: ProviderName): DeserializedAuthIdentity | undefined;
