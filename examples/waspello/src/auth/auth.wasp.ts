import { page, route, type Part } from "@wasp.sh/spec";

import LoginPage from "./LoginPage" with { type: "ref" };
import SignupPage from "./SignupPage" with { type: "ref" };

export const auth: Part[] = [
  route("SignupRoute", "/signup", page(SignupPage)),
  route("LoginRoute", "/login", page(LoginPage)),
];
