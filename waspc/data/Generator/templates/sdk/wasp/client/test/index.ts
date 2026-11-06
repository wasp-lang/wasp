import { initializeQueryClient } from "@wasp/lib-sdk-core/browser";
import { createMockServer, type MockServer } from "@wasp/lib-test-utils";
import { serialize } from "../../core/serialization/index.js";
import { config } from "../config.js";

// Actions wait for Wasp's shared query client when they finish. The app's entry
// point usually initializes it, but that doesn't run in tests.
initializeQueryClient();

// PUBLIC API
export { renderInContext, WaspTestWrapper } from "@wasp/lib-test-utils";

// PUBLIC API
/**
 * Starts an MSW server for the current test file and returns helpers for
 * mocking Wasp queries and APIs. Mocks are reset after each test.
 *
 * Call it once, at the top level of a test file.
 */
export function mockServer(): MockServer {
  return createMockServer({ apiUrl: config.apiUrl, serialize });
}
