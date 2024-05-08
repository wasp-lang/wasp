import type { OnAfterOAuthTokenReceivedHookFn, OnAfterSignupHookFn, OnBeforeOAuthRedirectHookFn, OnBeforeSignupHookFn } from 'wasp/server/auth';
export declare const onBeforeSignup: OnBeforeSignupHookFn;
export declare const onAfterSignup: OnAfterSignupHookFn;
export declare const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHookFn;
export declare const onAfterOAuthTokenReceived: OnAfterOAuthTokenReceivedHookFn;
