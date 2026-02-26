export const SubscriptionStatus = {
  Active: "active",
  PastDue: "past_due",
  CancelAtPeriodEnd: "cancel_at_period_end",
  Deleted: "deleted",
} as const;

export type SubscriptionStatus =
  (typeof SubscriptionStatus)[keyof typeof SubscriptionStatus];

export function isActiveSubscription(status: SubscriptionStatus | null | undefined): boolean {
  return (
    status === SubscriptionStatus.Active ||
    status === SubscriptionStatus.CancelAtPeriodEnd
  );
}
