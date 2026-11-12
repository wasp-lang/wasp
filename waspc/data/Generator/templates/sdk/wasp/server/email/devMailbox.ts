{{={= =}=}}
import { DEV_MAILBOX_API_BASE_PATH } from "@wasp/lib-dummy-email-sender/node";

// PUBLIC API
export type {
  DevMailboxEmail,
  DevMailboxEmailSummary,
} from "@wasp/lib-dummy-email-sender/node";

// PUBLIC API
export const devMailboxApiPath = `{= devMailboxRoutePath =}${DEV_MAILBOX_API_BASE_PATH}`;
