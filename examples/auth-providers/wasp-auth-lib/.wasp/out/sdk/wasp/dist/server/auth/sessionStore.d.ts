/**
 * Wasp's own session store: every session the app runs on -- whichever auth
 * provider verified the login -- is a row in the app's `Session`
 * table, created and revoked through this module.
 *
 * Lucia is the implementation detail behind it. Nothing outside this module may
 * touch Lucia for session work, so migrating off it is a one-module change.
 */
export type StoredSession = {
    id: string;
    authId: string;
    /**
     * Id of the provider that minted this session ('wasp', 'external:clerk',
     * ...). Every session records it at mint time, so logout revocation and
     * `user.sessionProviderId` always know which provider vouched for the
     * login. Null only on rows from before the column existed; `validateSession`
     * treats those as invalid.
     */
    providerId: string | null;
    /**
     * The external provider's own session id when this session was minted by
     * credential exchange (`POST /auth/login/:providerId`); lets logout revoke
     * the provider's session too (dual sign-out). Null for sessions issued by
     * Wasp's own auth.
     */
    providerSessionId: string | null;
};
export declare function getBearerToken(header: string | null | undefined): string | null;
export declare function createSession(authId: string, options: {
    providerId: string;
    providerSessionId?: string;
}): Promise<{
    id: string;
}>;
export declare function validateSession(token: string): Promise<(StoredSession & {
    providerId: string;
}) | null>;
export declare function getStoredSession(sessionId: string): Promise<StoredSession | null>;
export declare function getStoredSessionsForAuthId(authId: string): Promise<StoredSession[]>;
export declare function revokeSession(sessionId: string): Promise<void>;
export declare function revokeAllSessions(authId: string): Promise<void>;
//# sourceMappingURL=sessionStore.d.ts.map