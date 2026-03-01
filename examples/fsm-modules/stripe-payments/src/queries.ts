import type { AuthenticatedQuery } from "wasp/server/module";
import { requireUser } from "./auth.js";
import type { Entities } from "./store.js";

type SubscriptionStatusResult = {
  subscriptionStatus: string | null;
  subscriptionPlan: string | null;
  datePaid: Date | null;
  hasStripeCustomer: boolean;
};

export const getSubscriptionStatus: AuthenticatedQuery<Entities, void, SubscriptionStatusResult> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  const dbUser = await context.entities.User.findUniqueOrThrow({
    where: { id: user.id },
  });
  return {
    subscriptionStatus: dbUser.subscriptionStatus,
    subscriptionPlan: dbUser.subscriptionPlan,
    datePaid: dbUser.datePaid,
    hasStripeCustomer: !!dbUser.stripeCustomerId,
  };
};
