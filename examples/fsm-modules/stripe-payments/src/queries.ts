import type { AuthenticatedQueryDefinition } from "wasp/server/module";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { userEntityName } = moduleConfig;

type SubscriptionStatus = {
  subscriptionStatus: string | null;
  subscriptionPlan: string | null;
  datePaid: Date | null;
  hasStripeCustomer: boolean;
};

export const getSubscriptionStatus: AuthenticatedQueryDefinition<void, SubscriptionStatus> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  const dbUser = await context.entities[userEntityName].findUniqueOrThrow({
    where: { id: user.id },
  });
  return {
    subscriptionStatus: dbUser.subscriptionStatus as string | null,
    subscriptionPlan: dbUser.subscriptionPlan as string | null,
    datePaid: dbUser.datePaid as Date | null,
    hasStripeCustomer: !!dbUser.stripeCustomerId,
  };
};
