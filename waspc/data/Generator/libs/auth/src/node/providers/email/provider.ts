import { defineHandler, type EmailFromField } from "@wasp.sh/lib-sdk-core/node";
import { Router } from "express";
import type { ProviderName } from "../../../providerData.js";
import type { AuthHookTypes, AuthHooks } from "../../hooks.js";
import type { AuthIdentities, UserSignupFields } from "../../identities.js";
import type { JwtHelpers } from "../../jwt.js";
import type { ProviderConfig } from "../../providers.js";
import type { Sessions } from "../../sessions.js";
import { getLoginRoute } from "./login.js";
import { getRequestPasswordResetRoute } from "./requestPasswordReset.js";
import { resetPassword } from "./resetPassword.js";
import { getSignupRoute } from "./signup.js";
import type {
  GetPasswordResetEmailContentFn,
  GetVerificationEmailContentFn,
} from "./types.js";
import type { AuthEmail, EmailVerification } from "./verification.js";
import { verifyEmail } from "./verifyEmail.js";

/** What the email auth flows need from the framework and the app. */
export type EmailAuthDependencies<H extends AuthHookTypes> = {
  identities: AuthIdentities<H["entities"]>;
  sessions: Sessions<unknown>;
  hooks: AuthHooks<H>;
  jwt: JwtHelpers;
  emailVerification: EmailVerification<Required<AuthEmail>>;
};

export type EmailAuthConfig = {
  id: ProviderName;
  displayName: string;
  userSignupFields: UserSignupFields | undefined;
  fromField: EmailFromField;
  emailVerificationClientRoute: string;
  passwordResetClientRoute: string;
  getVerificationEmailContent: GetVerificationEmailContentFn;
  getPasswordResetEmailContent: GetPasswordResetEmailContentFn;
  isEmailAutoVerified: boolean;
};

export function createEmailAuthProvider<H extends AuthHookTypes>(
  config: EmailAuthConfig,
  dependencies: EmailAuthDependencies<H>,
): ProviderConfig {
  return {
    id: config.id,
    displayName: config.displayName,
    createRouter() {
      const router = Router();

      const loginRoute = defineHandler(getLoginRoute(dependencies));
      router.post("/login", loginRoute);

      const signupRoute = defineHandler(
        getSignupRoute(
          {
            userSignupFields: config.userSignupFields,
            fromField: config.fromField,
            clientRoute: config.emailVerificationClientRoute,
            getVerificationEmailContent: config.getVerificationEmailContent,
            isEmailAutoVerified: config.isEmailAutoVerified,
          },
          dependencies,
        ),
      );
      router.post("/signup", signupRoute);

      const requestPasswordResetRoute = defineHandler(
        getRequestPasswordResetRoute(
          {
            fromField: config.fromField,
            clientRoute: config.passwordResetClientRoute,
            getPasswordResetEmailContent: config.getPasswordResetEmailContent,
          },
          dependencies,
        ),
      );
      router.post("/request-password-reset", requestPasswordResetRoute);

      router.post(
        "/reset-password",
        defineHandler(resetPassword(dependencies)),
      );
      router.post("/verify-email", defineHandler(verifyEmail(dependencies)));

      return router;
    },
  };
}
