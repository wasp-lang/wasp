import { page, route } from "@wasp.sh/spec";

import LoginPage from "@src/auth/LoginPage";
import SignupPage from "@src/auth/SignupPage";

export const auth = [
  route("SignupRoute", "/signup", page(SignupPage)),
  route("LoginRoute", "/login", page(LoginPage)),
];
