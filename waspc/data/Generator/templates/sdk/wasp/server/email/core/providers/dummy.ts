import { createDummyEmailSender } from "@wasp/lib-dummy-email-sender/node";

import config from "../../../config.js";
import { devMailbox, devMailboxRoutePath } from "../devMailboxInstance.js";
import { getDefaultFromField } from "../helpers.js";
import type { DummyEmailProvider, EmailSender } from "../types";

// PRIVATE API
export function initDummyEmailSender(
  _config?: DummyEmailProvider,
): EmailSender {
  return createDummyEmailSender({
    mailbox: devMailbox,
    defaultFrom: getDefaultFromField(),
    mailboxUrl: `${config.serverUrl}${devMailboxRoutePath}`,
  });
}
