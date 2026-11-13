import { api, type Spec } from "@wasp/spec";
import { createTestUser } from "./api" with { type: "ref" };

// Test fixtures only; disabled unless LINKLY_ENABLE_TEST_API=true (never in production).
export const testingSpec: Spec = [
  api("POST", "/__test/users", createTestUser, { auth: false }),
];
