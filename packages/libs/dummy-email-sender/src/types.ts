// TODO: Import from the email sender interface once Wasp has one, see
// https://github.com/wasp-lang/wasp/issues/2881.
// Copy of `EmailFromField` from `@wasp/lib-sdk-core/node`, so this lib doesn't
// depend on it.
export type EmailFromField = {
  name?: string;
  email: string;
};
