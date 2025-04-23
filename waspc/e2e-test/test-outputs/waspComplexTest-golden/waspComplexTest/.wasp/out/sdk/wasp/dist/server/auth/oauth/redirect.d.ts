export declare const loginPath = "login";
export declare const exchangeCodeForTokenPath = "exchange-code";
export declare const callbackPath = "callback";
export declare function getRedirectUriForOneTimeCode(oneTimeCode: string): URL;
export declare function handleOAuthErrorAndGetRedirectUri(error: unknown): URL;
export declare function getRedirectUriForCallback(providerName: string): URL;
//# sourceMappingURL=redirect.d.ts.map