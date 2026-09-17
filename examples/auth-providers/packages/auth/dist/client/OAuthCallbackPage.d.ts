/**
 * The client route the OAuth handback lands on: redeems the one-time code,
 * then redirects. After an account-linking flow (`startOAuthLink`) there is
 * no code to redeem: the user is refreshed and sent to `linkedRedirectTo`.
 */
export declare function OAuthCallbackPage({ linkedRedirectTo, }?: {
    /** Where to go after a provider was connected. Default: `onAuthSucceededRedirectTo`. */
    linkedRedirectTo?: string;
}): import("react").JSX.Element;
