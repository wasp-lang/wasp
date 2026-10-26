import { type Spec, page, route } from "@wasp.sh/spec";
import { WelcomePage } from "./WelcomePage" with { type: "ref" };

export const welcomeSpec: Spec = [
  route("WelcomeRoute", "/", page(WelcomePage)),
];
