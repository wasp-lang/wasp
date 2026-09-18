export {
  type GetPasswordResetEmailContentFn,
  type GetVerificationEmailContentFn,
} from "./auth/email.js";
export { defineProvider } from "./auth/oauth/provider.js";
export { type ProviderConfig, type RequestWithWasp } from "./auth/providers.js";
export {
  createInvalidCredentialsError,
  doFakeWork,
  sanitizeAndSerializeProviderData,
} from "./auth/utils.js";

export { ensurePasswordIsHashed } from "./auth/utils.js";
