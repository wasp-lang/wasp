import type { DevMailboxEmail, DevMailboxEmailSummary } from "../core/email.js";
import type { DevMailboxEmailFilter } from "../core/emailFilter.js";
import type { ApiErrorCode } from "./apiError.js";

export type ListEmailsResponse = { emails: DevMailboxEmailSummary[] };

export type EmailResponse = { email: DevMailboxEmail };

export type WaitForEmailRequest = DevMailboxEmailFilter & {
  timeoutMs?: number;
};

export type DeleteEmailRequest = { id: string };

export type ActionResponse = { ok: true };

export type ApiErrorResponse = {
  error: { code: ApiErrorCode; message: string };
};
