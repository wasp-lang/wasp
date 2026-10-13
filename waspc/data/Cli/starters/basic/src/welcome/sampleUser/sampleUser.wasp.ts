import { type Spec, api } from "@wasp.sh/spec";
import { createSampleUser } from "./apis" with { type: "ref" };

// Wasp evaluates the spec with NODE_ENV set to "development" for `wasp start`
// and "production" for `wasp build`, so this endpoint never reaches production.
// See https://wasp.sh/docs/features/spec#detecting-production-mode
export const sampleUserSpec: Spec =
  process.env.NODE_ENV === "development"
    ? [api("POST", "/dev/sample-user", createSampleUser, { auth: false })]
    : [];
