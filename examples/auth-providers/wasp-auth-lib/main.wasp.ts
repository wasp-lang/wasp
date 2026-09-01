import { action, app, page, query, route } from "@wasp.sh/spec";
import { waspAuthLib } from "@wasp.sh/auth/spec";
import { MainPage } from "./src/MainPage" with { type: "ref" };
import { LoginPage } from "./src/auth/LoginPage" with { type: "ref" };
import { OAuthCallback } from "./src/auth/OAuthCallback" with { type: "ref" };
import { EmailVerified } from "./src/auth/EmailVerified" with { type: "ref" };
import { PasswordReset } from "./src/auth/PasswordReset" with { type: "ref" };
import { createTask, getMyTasks } from "./src/operations" with { type: "ref" };
import { onAfterLogin, onBeforeSignup } from "./src/auth/hooks" with { type: "ref" };

export default app({
  name: "authProviderWaspAuthLib",
  wasp: { version: "^0.26.0" },
  title: "Auth providers — full Wasp auth as a package",

  // The email method's verification and reset mail goes through this, via the
  // package's `email-send` grant (dropping emailSender would be a compile
  // error while the grant is requested).
  emailSender: {
    provider: "Dummy",
    defaultFrom: { name: "Wasp Auth Lib", email: "auth@example.com" },
  },

  auth: {
    userEntity: "User",
    onAuthFailedRedirectTo: "/login",
    // All of Wasp's own auth -- username & password, email with verification
    // and password reset, Google OAuth -- externalized into the
    // @wasp.sh/auth package and plugged back in through the provider
    // contract. Each method records identities under its own namespace
    // (external:wasp-auth/username, /email, /google).
    providers: [
      waspAuthLib({
        methods: {
          usernameAndPassword: {},
          email: {
            emailVerificationPath: "/email-verified",
            passwordResetPath: "/password-reset",
          },
          google: {},
        },
        oauthCallbackPath: "/oauth/callback",
      }),
    ],
    // App-level lifecycle hooks: fired at Wasp-owned choke points for every
    // provider, so they cover the package's flows -- the adapter neither
    // calls nor can skip them. onBeforeSignup vetoes names containing
    // "blocked".
    hooks: {
      onBeforeSignup,
      onAfterLogin,
    },
  },

  spec: [
    route("MainRoute", "/", page(MainPage, { authRequired: true })),
    route("LoginRoute", "/login", page(LoginPage)),
    // The pages the package's flows land on: the OAuth one-time-code
    // redemption, the emailed verification link, the emailed reset link. The
    // components come from @wasp.sh/auth/client; the app only mounts them.
    route("OAuthCallbackRoute", "/oauth/callback", page(OAuthCallback)),
    route("EmailVerifiedRoute", "/email-verified", page(EmailVerified)),
    route("PasswordResetRoute", "/password-reset", page(PasswordReset)),
    query(getMyTasks, { entities: ["Task"], auth: true }),
    action(createTask, { entities: ["Task"], auth: true }),
  ],
});
