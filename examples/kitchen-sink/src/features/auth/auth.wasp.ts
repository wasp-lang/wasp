import { action, page, route, type Auth, type Decl } from "@wasp.sh/spec";

import { customSignup } from "./customSignup" with { type: "ref" };
import {
  onAfterEmailVerified,
  onAfterLogin,
  onAfterSignup,
  onBeforeLogin,
  onBeforeSignup,
} from "./hooks" with { type: "ref" };
import { CustomSignupPage } from "./pages/CustomSignupPage" with { type: "ref" };
import { EmailVerification } from "./pages/EmailVerification" with { type: "ref" };
import Login from "./pages/Login" with { type: "ref" };
import { ManualSignupPage } from "./pages/ManualSignupPage" with { type: "ref" };
import { PasswordReset } from "./pages/PasswordReset" with { type: "ref" };
import { ProfilePage } from "./pages/ProfilePage" with { type: "ref" };
import { RequestPasswordReset } from "./pages/RequestPasswordReset" with { type: "ref" };
import Signup from "./pages/Signup" with { type: "ref" };
import {
  discordConfig,
  discordUserSignupFields,
} from "./providers/discord" with { type: "ref" };
import {
  emailUserSignupFields,
  getPasswordResetEmailContent,
  getVerificationEmailContent,
} from "./providers/email" with { type: "ref" };
import {
  gitHubConfig,
  gitHubUserSignupFields,
} from "./providers/github" with { type: "ref" };
import {
  googleConfig,
  googleUserSignupFields,
} from "./providers/google" with { type: "ref" };
import {
  microsoftConfig,
  microsoftUserSignupFields,
} from "./providers/microsoft" with { type: "ref" };
import {
  slackConfig,
  slackUserSignupFields,
} from "./providers/slack" with { type: "ref" };

export const authConfig: Auth = {
  userEntity: "User",
  methods: {
    slack: {
      configFn: slackConfig,
      userSignupFields: slackUserSignupFields,
    },
    discord: {
      configFn: discordConfig,
      userSignupFields: discordUserSignupFields,
    },
    google: {
      configFn: googleConfig,
      userSignupFields: googleUserSignupFields,
    },
    gitHub: {
      configFn: gitHubConfig,
      userSignupFields: gitHubUserSignupFields,
    },
    microsoft: {
      configFn: microsoftConfig,
      userSignupFields: microsoftUserSignupFields,
    },
    email: {
      userSignupFields: emailUserSignupFields,
      fromField: {
        name: "Wasp Kitchen Sink",
        email: "kitchen-sink@wasp.sh",
      },
      emailVerification: {
        getEmailContentFn: getVerificationEmailContent,
        clientRoute: "EmailVerificationRoute",
      },
      passwordReset: {
        getEmailContentFn: getPasswordResetEmailContent,
        clientRoute: "PasswordResetRoute",
      },
    },
  },
  onAuthFailedRedirectTo: "/login",
  onAuthSucceededRedirectTo: "/",
  onBeforeSignup,
  onAfterSignup,
  onAfterEmailVerified,
  onBeforeLogin,
  onAfterLogin,
};

export const auth: Decl[] = [
  route("SignupRoute", "/signup", page(Signup)),
  route("LoginRoute", "/login", page(Login)),
  route("PasswordResetRoute", "/password-reset", page(PasswordReset)),
  route(
    "EmailVerificationRoute",
    "/email-verification-",
    page(EmailVerification),
  ),
  route(
    "RequestPasswordResetRoute",
    "/request-password-reset",
    page(RequestPasswordReset),
  ),
  route("ProfileRoute", "/profile", page(ProfilePage, { authRequired: true })),
  route("ManualSignupRoute", "/manual-signup", page(ManualSignupPage)),
  route("CustomSignupRoute", "/custom-signup", page(CustomSignupPage)),
  action(customSignup),
];
