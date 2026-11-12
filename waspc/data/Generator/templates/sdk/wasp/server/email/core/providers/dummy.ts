import { createDummyEmailSender } from "@wasp/lib-dummy-email-sender/node";

import { getDefaultFromField } from "../helpers.js";
import type { DummyEmailProvider, EmailSender } from "../types";

// PRIVATE API
export function initDummyEmailSender(
  _config?: DummyEmailProvider,
): EmailSender {
  return createDummyEmailSender({ defaultFrom: getDefaultFromField() });
}
