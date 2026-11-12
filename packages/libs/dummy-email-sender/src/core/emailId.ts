import { randomBytes } from "node:crypto";

/**
 * Ids start with the send time, so they sort chronologically and work as file
 * names, e.g. `2026-11-11T10-20-30-123Z-3f9a1c`.
 */
const EMAIL_ID_PATTERN =
  /^\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}-\d{3}Z-[0-9a-f]+$/;

export function isEmailId(value: string): boolean {
  return EMAIL_ID_PATTERN.test(value);
}

export function generateEmailId(sentAt: Date): string {
  const timestamp = sentAt.toISOString().replace(/[:.]/g, "-");
  const suffix = randomBytes(3).toString("hex");
  return `${timestamp}-${suffix}`;
}

/** Emails sent in the same millisecond still get increasing times. */
export function createMonotonicClock(): () => Date {
  let lastTime = 0;
  return (): Date => {
    lastTime = Math.max(Date.now(), lastTime + 1);
    return new Date(lastTime);
  };
}
