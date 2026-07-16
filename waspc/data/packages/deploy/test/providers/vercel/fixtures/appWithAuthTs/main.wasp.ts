import { app, auth, page, route } from "@wasp.sh/spec";

import { LoginPage } from "./src/pages/LoginPage";

export default app({
  name: "AppWithAuthTs",
  wasp: { version: "0.25.0" },
  spec: [
    auth({
      userEntity: "User",
      methods: { usernameAndPassword: {} },
      onAuthFailedRedirectTo: "/login",
    }),
    route("LoginRoute", "/login", page(LoginPage)),
  ],
});
