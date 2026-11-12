{{={= =}=}}
import {
  createDevMailboxApiClient,
  DEV_MAILBOX_API_BASE_PATH,
  type DevMailboxClient,
} from "@wasp/lib-dummy-email-sender/node";

// PUBLIC API
export type {
  DevMailboxClient,
  DevMailboxClientWaitForOptions,
  DevMailboxEmail,
  DevMailboxEmailFilter,
  DevMailboxEmailSummary,
  DevMailboxSearchField,
} from "@wasp/lib-dummy-email-sender/node";

// PUBLIC API
export { ApiError as DevMailboxApiError } from "@wasp/lib-dummy-email-sender/node";

// PUBLIC API
export const devMailboxApiPath = `{= devMailboxRoutePath =}${DEV_MAILBOX_API_BASE_PATH}`;

// PUBLIC API
/**
 * A typed client for the dev mailbox HTTP API, e.g. for end-to-end tests.
 *
 * @param serverUrl The Wasp server's URL, e.g. `http://localhost:3001`.
 */
export function createDevMailboxClient(serverUrl: string): DevMailboxClient {
  return createDevMailboxApiClient(serverUrl + devMailboxApiPath);
}
