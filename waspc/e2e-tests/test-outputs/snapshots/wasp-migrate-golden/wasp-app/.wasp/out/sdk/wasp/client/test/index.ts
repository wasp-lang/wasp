import { createMockServer, type MockServer } from "@wasp.sh/lib-test-utils";
import { serialize } from "../../core/serialization/index.js";
import { config } from "../config.js";

// PUBLIC API
export { renderInContext } from "@wasp.sh/lib-test-utils";

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
