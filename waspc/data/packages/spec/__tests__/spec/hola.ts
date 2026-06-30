import { app, page, ref, route } from "../../src/spec/publicApi/index.js";

export default app({
  name: "hola",
  wasp: { version: "^0.25.0" },
  title: "My Hola App",
  auth: {
    userEntity: "User",
    methods: {
      email: {
        fromField: { email: "my@app.com" },
        // Routes are defined inline, right where they are referenced.
        emailVerification: {
          clientRoute: route(
            "EmailVerifyRoute",
            "/email-verify",
            page(
              ref({ import: "EmailVerifyPage", from: "./src/EmailVerifyPage" }),
            ),
          ),
        },
        passwordReset: {
          clientRoute: route(
            "PasswordResetRoute",
            "/password-reset",
            page(
              ref({
                import: "PasswordResetPage",
                from: "./src/PasswordResetPage",
              }),
            ),
          ),
        },
      },
    },
    onAuthFailedRedirectTo: route(
      "LoginRoute",
      "/login",
      page(ref({ import: "LoginPage", from: "./src/LoginPage" })),
    ),
    onAuthSucceededRedirectTo: route(
      "HomeRoute",
      "/",
      page(ref({ import: "HomePage", from: "./src/HomePage" })),
    ),
  },
  // `spec` is empty: every route above is referenced inline by `auth` and gets
  // auto-registered, the same way a page passed to `route()` is registered.
  spec: [],
});
