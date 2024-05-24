import type { AuthUserData, AuthUser, MinimalUserEntityWithAuth } from '../server/auth/user.js';
/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */
export declare function getEmail(user: MinimalUserEntityWithAuth): string | null;
export declare function getUsername(user: MinimalUserEntityWithAuth): string | null;
export declare function getFirstProviderUserId(user?: MinimalUserEntityWithAuth): string | null;
export type { AuthUserData, AuthUser } from '../server/auth/user.js';
export declare function makeAuthUserIfPossible(user: null): null;
export declare function makeAuthUserIfPossible(user: AuthUserData): AuthUser;
