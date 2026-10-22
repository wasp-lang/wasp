import { type Spec, api, page, route } from "@wasp.sh/spec";
import { createSampleUser } from "./sampleUser/apis" with { type: "ref" };
import { WelcomePage } from "./WelcomePage" with { type: "ref" };

// The sample user API must never reach production, so the whole welcome page
// is development-only. Wasp evaluates the spec with NODE_ENV set to
// "development" for `wasp start` and "production" for `wasp build`.
// See https://wasp.sh/docs/features/spec#detecting-production-mode
if (process.env.NODE_ENV !== "development") {
  throw new Error(
    "The welcome page in `src/welcome/` is only meant for development. " +
      "Before building for production, remove it by following the " +
      '"Removing the welcome page" steps in `README.md`.',
  );
}

export const welcomeSpec: Spec = [
  route("WelcomeRoute", "/", page(WelcomePage)),
  api("POST", "/dev/sample-user", createSampleUser, { auth: false }),
];
