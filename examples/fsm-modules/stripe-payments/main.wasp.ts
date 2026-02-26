import { App } from "wasp-config";
import { createStripePaymentsModule } from "./src/index.js";

const app = new App("stripePaymentsDev", {
  title: "Stripe Payments Module Dev",
  wasp: { version: "^0.21.2" },
});

app.emailSender({ provider: "Dummy" });

app.auth({
  userEntity: "User",
  methods: {
    email: {
      fromField: {
        name: "Stripe Payments Dev",
        email: "noreply@waspello.dev",
      },
      emailVerification: {
        clientRoute: "EmailVerificationRoute",
      },
      passwordReset: {
        clientRoute: "PasswordResetRoute",
      },
    },
  },
  onAuthFailedRedirectTo: "/login",
});

const loginPage = app.page("LoginPage", {
  component: { import: "LoginPage", from: "@src/scaffold/LoginPage" },
});
app.route("LoginRoute", { path: "/login", to: loginPage });

const signupPage = app.page("SignupPage", {
  component: { import: "SignupPage", from: "@src/scaffold/SignupPage" },
});
app.route("SignupRoute", { path: "/signup", to: signupPage });

app.route("EmailVerificationRoute", {
  path: "/email-verification",
  to: app.page("EmailVerification", {
    component: { import: "LoginPage", from: "@src/scaffold/LoginPage" },
  }),
});

app.route("PasswordResetRoute", {
  path: "/password-reset",
  to: app.page("PasswordReset", {
    component: { import: "LoginPage", from: "@src/scaffold/LoginPage" },
  }),
});

app.use(
  createStripePaymentsModule({
    userEntityName: "User",
    premiumPlanPriceId: process.env.STRIPE_PREMIUM_PRICE_ID!,
    subscriptionRoute: "/",
  }),
);

export default app;
