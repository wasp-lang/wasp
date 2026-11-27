import { page, route, type Spec } from "@wasp/spec";
import { LoginPage, SignupPage } from "./AuthPages" with { type: "ref" };
import { HomePage } from "./HomePage" with { type: "ref" };

export const appSpec: Spec = [
  // Prerendered, so an agent that fetches the dashboard's address without
  // running JavaScript still reads "For AI agents". It's the logged-out view.
  route("HomeRoute", "/", page(HomePage), { prerender: true }),
  route("LoginRoute", "/login", page(LoginPage)),
  route("SignupRoute", "/signup", page(SignupPage)),
];
