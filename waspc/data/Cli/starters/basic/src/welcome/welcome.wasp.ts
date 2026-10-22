import { type Spec, page, route } from "@wasp.sh/spec";
import { WelcomePage } from "./WelcomePage" with { type: "ref" };

// Wasp evaluates the spec with NODE_ENV set to "development" for `wasp start`
// and "production" for `wasp build`.
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
];
