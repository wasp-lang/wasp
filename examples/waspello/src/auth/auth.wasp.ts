import { page, route, type Spec } from "@wasp.sh/spec";

import LoginPage from "./LoginPage" with { type: "ref" };
import SignupPage from "./SignupPage" with { type: "ref" };

// We also want to use it in the `auth` configuration, so we export it.
export const loginRoute = route("LoginRoute", "/login", page(LoginPage));

export const authSpec: Spec = [
  route("SignupRoute", "/signup", page(SignupPage)),
  loginRoute,
];
