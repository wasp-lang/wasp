import type { Clock } from "../authService/runtime";
import type { EmailProviderData } from "../providerData";

export function getEmailVerificationResendStatus({
  providerData,
  clock,
}: {
  providerData: EmailProviderData;
  clock: Clock;
}) {
  return getEmailResendStatus({
    sentAt: providerData.emailVerificationSentAt,
    clock,
  });
}

export function getPasswordResetResendStatus({
  providerData,
  clock,
}: {
  providerData: EmailProviderData;
  clock: Clock;
}) {
  return getEmailResendStatus({
    sentAt: providerData.passwordResetSentAt,
    clock,
  });
}

export function getEmailResendStatus({
  sentAt,
  clock,
  resendInterval = 1000 * 60,
}: {
  sentAt: string | null;
  clock: Clock;
  resendInterval?: number;
}): {
  isResendAllowed: boolean;
  timeLeft: number;
} {
  if (!sentAt) {
    return {
      isResendAllowed: true,
      timeLeft: 0,
    };
  }

  const diff = clock.now().getTime() - new Date(sentAt).getTime();
  const isResendAllowed = diff > resendInterval;
  const timeLeft = isResendAllowed
    ? 0
    : Math.round((resendInterval - diff) / 1000);
  return { isResendAllowed, timeLeft };
}
