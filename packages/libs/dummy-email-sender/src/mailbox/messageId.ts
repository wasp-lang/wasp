import { randomBytes } from "node:crypto";

/**
 * Message ids start with the send time, so sorting them as strings sorts the
 * messages chronologically. They are also safe to use as file names.
 *
 * Example: `2026-11-11T10-20-30-123Z-3f9a1c`
 */
const MESSAGE_ID_PATTERN =
  /^\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}-\d{3}Z-[0-9a-f]+$/;

export function isMessageId(value: string): boolean {
  return MESSAGE_ID_PATTERN.test(value);
}

export function generateMessageId(sentAt: Date): string {
  const timestamp = sentAt.toISOString().replace(/[:.]/g, "-");
  const suffix = randomBytes(3).toString("hex");
  return `${timestamp}-${suffix}`;
}

/**
 * Returns send times that never go backwards or repeat within a process, so
 * two emails sent in the same millisecond keep their order.
 */
export function createMonotonicClock(now: () => Date = () => new Date()) {
  let lastTime = 0;
  return (): Date => {
    lastTime = Math.max(now().getTime(), lastTime + 1);
    return new Date(lastTime);
  };
}
