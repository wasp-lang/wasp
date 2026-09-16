import { getClientOptions } from "./runtime.js";
import type { OAuthProviderName } from "./types.js";
export declare function login(data: {
    username: string;
    password: string;
} | {
    email: string;
    password: string;
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
export declare function isMethodEnabled(name: keyof ReturnType<typeof getClientOptions>["methods"]): boolean;
