/**
 * Runtime-agnostic helpers over the identities Wasp's own auth records.
 * Usable from both the server (`context.user`) and the client (`useAuth()`).
 */
type UserWithIdentities = {
    auth?: {
        identities: Array<{
            providerName: string;
            providerUserId: string;
        }>;
    } | null;
    identities?: Array<{
        providerName: string;
        providerUserId: string;
    }>;
};
export declare function getEmail(user: UserWithIdentities): string | null;
export declare function getUsername(user: UserWithIdentities): string | null;
export {};
