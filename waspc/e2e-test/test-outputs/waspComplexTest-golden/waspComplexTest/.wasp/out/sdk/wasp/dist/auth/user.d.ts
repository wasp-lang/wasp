import type { AuthUserData, AuthUser, UserEntityWithAuth } from '../server/auth/user.js';
/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */
export declare function getEmail(user: UserEntityWithAuth): string | null;
export declare function getUsername(user: UserEntityWithAuth): string | null;
export declare function getFirstProviderUserId(user?: UserEntityWithAuth): string | null;
export type { AuthUserData, AuthUser } from '../server/auth/user.js';
export declare function makeAuthUserIfPossible(user: null): null;
export declare function makeAuthUserIfPossible(user: AuthUserData): AuthUser;
export declare function makeAuthUserIfPossible(user: AuthUserData | null): AuthUser | null;
//# sourceMappingURL=user.d.ts.map