export { DEV_MAILBOX_API_BASE_PATH } from "./api/apiPaths.js";
export { createDevMailbox, type DevMailbox } from "./core/devMailbox.js";
export {
  type DevMailboxEmail,
  type DevMailboxEmailSummary,
} from "./core/email.js";
export { createDummyEmailSender } from "./dummyEmailSender.js";
export { registerDevMailboxRoutes } from "./routes.js";
