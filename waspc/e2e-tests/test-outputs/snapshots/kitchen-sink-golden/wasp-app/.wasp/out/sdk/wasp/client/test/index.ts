import {
  createMockServer,
  type MockServer,
} from "@wasp.sh/lib-sdk-core/browser/test";
import { serialize } from "../../core/serialization/index.js";
import { config } from "../config.js";

// PUBLIC API
export {
  renderInContext,
  WaspTestWrapper,
} from "@wasp.sh/lib-sdk-core/browser/test";

/**
 * Starts an MSW server for the current test file and returns helpers for
 * mocking Wasp queries and APIs. Mocks are reset after each test.
 *
 * Call it once, at the top level of a test file.
 */
// PUBLIC API
export function mockServer(): MockServer {
  return createMockServer({ apiUrl: config.apiUrl, serialize });
}
