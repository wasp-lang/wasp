import { getClientSpec } from "./runtime.js";
import type { OAuthProviderName } from "./types.js";
/**
 * `persistent: false` is a login without "remember me": the credential lasts
 * for the browser session only.
 */
export declare function login(data: ({
    username: string;
    password: string;
} | {
    email: string;
    password: string;
}) & {
    persistent?: boolean;
}): Promise<void>;
export declare function signup(data: ({
    username: string;
    password: string;
} | {
    email: string;
    password: string;
}) & Record<string, unknown>): Promise<{
    success: boolean;
}>;
export declare function requestPasswordReset(data: {
    email: string;
}): Promise<{
    success: boolean;
}>;
export declare function resetPassword(data: {
    token: string;
    password: string;
}): Promise<{
    success: boolean;
}>;
export declare function verifyEmail(data: {
    token: string;
}): Promise<{
    success: boolean;
    reason?: string;
}>;
export declare function exchangeOAuthCodeForSession(code: string): Promise<void>;
export declare function signInUrl(provider: OAuthProviderName): string;
export declare function isMethodEnabled(name: keyof ReturnType<typeof getClientSpec>["methods"]): boolean;
type LinkedIdentity = {
    providerName: string;
    providerUserId: string;
};
/**
 * `merge-required`: the login belongs to another account, and you just
 * proved it is yours. Ask the user, then pass the ticket to
 * {@link confirmMerge}. Only apps that declare `auth.mergeUsers` ever see it.
 */
export type LinkResult = {
    status: "linked";
} | {
    status: "merge-required";
    mergeTicket: string;
};
/** Adds a username and password to the signed-in user's account. */
export declare function linkUsername(data: {
    username: string;
    password: string;
}): Promise<LinkResult>;
/**
 * Adds an email and password to the signed-in user's account. The address
 * must be verified through the emailed link before it can be used to log in.
 */
export declare function linkEmail(data: {
    email: string;
    password: string;
}): Promise<LinkResult>;
/**
 * The second step of a merge, after the user agreed: the other account's
 * data and logins move into the signed-in one, and the other account is
 * deleted. Not reversible.
 */
export declare function confirmMerge(mergeTicket: string): Promise<void>;
/**
 * Disconnects one of `user.identities` from the signed-in user's account.
 * Rejects (409) when it is the account's only login method.
 */
export declare function unlink(identity: LinkedIdentity): Promise<void>;
/**
 * Sends the browser to the OAuth provider to connect it to the signed-in
 * user's account. A navigation cannot carry a bearer credential, so the
 * credential is first traded for a one-time code. There is none when the
 * credential is a cookie, which the navigation carries by itself.
 */
export declare function startOAuthLink(provider: OAuthProviderName): Promise<void>;
export {};
