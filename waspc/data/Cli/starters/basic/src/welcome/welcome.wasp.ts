import { type Spec, page, route } from "@wasp.sh/spec";
import { sampleUserSpec } from "./sampleUser/sampleUser.wasp";
import { WelcomePage } from "./WelcomePage" with { type: "ref" };

export const welcomeSpec: Spec = [
  route("WelcomeRoute", "/", page(WelcomePage)),
  sampleUserSpec,
];
