import type { ClientAdapterFactory } from "@wasp.sh/auth-contract/client";
import type { WaspAuthLibOptions } from "./spec.js";
export declare const createClientAdapter: ClientAdapterFactory<WaspAuthLibOptions>;
export declare function signup(username: string, password: string): Promise<void>;
export declare function login(username: string, password: string): Promise<void>;
export declare function emailSignup(email: string, password: string): Promise<void>;
export declare function emailLogin(email: string, password: string): Promise<void>;
export declare function verifyEmail(token: string): Promise<void>;
export declare function requestPasswordReset(email: string): Promise<void>;
export declare function resetPassword(token: string, password: string): Promise<void>;
/** Navigate here to start the Google login dance. */
export declare function googleLoginUrl(): string;
export declare function exchangeOAuthCode(code: string): Promise<void>;
export declare function AuthForm({ onSuccess }: {
    onSuccess?: () => void;
}): import("react").JSX.Element;
export declare function OAuthCallbackPage({ redirectTo }: {
    redirectTo?: string;
}): import("react").JSX.Element;
export declare function VerifyEmailPage({ loginPath }: {
    loginPath?: string;
}): import("react").JSX.Element;
export declare function PasswordResetPage({ loginPath }: {
    loginPath?: string;
}): import("react").JSX.Element;
