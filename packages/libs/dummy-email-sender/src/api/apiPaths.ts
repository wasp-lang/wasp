/** Relative to where the mailbox routes are mounted. */
export const DEV_MAILBOX_API_BASE_PATH = "/api";

/** Relative to `DEV_MAILBOX_API_BASE_PATH`. */
export const devMailboxApiPaths = {
  emails: "/emails",
  email: (id: string) => `/emails/${id}`,
  emailHtml: (id: string) => `/emails/${id}/html`,
  events: "/events",
  waitForEmail: "/emails.waitFor",
  deleteEmail: "/emails.delete",
  clearEmails: "/emails.clear",
} as const;
