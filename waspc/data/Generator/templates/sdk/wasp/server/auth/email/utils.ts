import { createEmailVerification } from "@wasp.sh/lib-auth/node";
import { emailSender } from "../../email/index.js";
import { config as waspServerConfig } from "../../index.js";
import { jwt } from "../jwt.js";
import { authIdentities } from "../utils.js";

// PUBLIC API
export { isEmailResendAllowed } from "@wasp.sh/lib-auth";

// PRIVATE API
export const emailVerification = createEmailVerification({
  frontendUrl: waspServerConfig.frontendUrl,
  jwt,
  emailSender,
  identities: authIdentities,
});

// PUBLIC API
export const {
  createEmailVerificationLink,
  createPasswordResetLink,
  sendPasswordResetEmail,
  sendEmailVerificationEmail,
} = emailVerification;
