/** The entity shape this module requires. */
export type User = {
  id: number;
  email: string | null;
  stripeCustomerId: string | null;
  subscriptionStatus: string | null;
  subscriptionPlan: string | null;
  datePaid: Date | null;
};

export type Entities = { User: User };
