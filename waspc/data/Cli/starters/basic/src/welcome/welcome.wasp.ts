import { type Spec, page, route } from "@wasp/spec";
import { WelcomePage } from "./WelcomePage" with { type: "ref" };

export const welcomeSpec: Spec = [
  route("WelcomeRoute", "/", page(WelcomePage)),
];
