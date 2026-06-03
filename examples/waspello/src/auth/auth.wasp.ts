import { page, route, type Decl } from "@wasp.sh/spec";

import LoginPage from "./LoginPage" with { type: "ref" };
import SignupPage from "./SignupPage" with { type: "ref" };

export const authDecls: Decl[] = [
  route("SignupRoute", "/signup", page(SignupPage)),
  route("LoginRoute", "/login", page(LoginPage)),
];
