export function isEmailResendAllowed<
  Field extends "emailVerificationSentAt" | "passwordResetSentAt",
>(
  fields: {
    [field in Field]: string | null;
  },
  field: Field,
  resendInterval: number = 1000 * 60,
): {
  isResendAllowed: boolean;
  timeLeft: number;
} {
  const sentAt = fields[field];
  if (!sentAt) {
    return {
      isResendAllowed: true,
      timeLeft: 0,
    };
  }
  const now = new Date();
  const diff = now.getTime() - new Date(sentAt).getTime();
  const isResendAllowed = diff > resendInterval;
  // Time left in seconds
  const timeLeft = isResendAllowed
    ? 0
    : Math.round((resendInterval - diff) / 1000);
  return { isResendAllowed, timeLeft };
}
