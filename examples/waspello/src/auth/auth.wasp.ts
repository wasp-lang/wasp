import { page, route, type Spec } from "@wasp.sh/spec";

import LoginPage from "./LoginPage" with { type: "ref" };
import SignupPage from "./SignupPage" with { type: "ref" };

export const authSpec: Spec = [
  route("SignupRoute", "/signup", page(SignupPage)),
  route("LoginRoute", "/login", page(LoginPage)),
];
