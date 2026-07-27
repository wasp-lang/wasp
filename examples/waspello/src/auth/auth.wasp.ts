import { page, route, type Auth, type Spec } from "@wasp.sh/spec";

import LoginPage from "./LoginPage" with { type: "ref" };
import SignupPage from "./SignupPage" with { type: "ref" };

const loginRoute = route("LoginRoute", "/login", page(LoginPage));

export const authConfig: Auth = {
  userEntity: "User",
  methods: {
    usernameAndPassword: {},
    google: {},
  },
  onAuthFailedRedirectTo: loginRoute,
};

export const authSpec: Spec = [
  route("SignupRoute", "/signup", page(SignupPage)),
  loginRoute,
];
