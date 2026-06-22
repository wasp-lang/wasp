export type OAuthRedirects = {
  getRedirectUrlForOneTimeCode(code: string): URL;
  getFailureRedirectUrl(error: unknown): URL;
};
