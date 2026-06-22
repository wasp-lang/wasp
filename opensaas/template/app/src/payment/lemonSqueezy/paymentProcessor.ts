import { lemonSqueezySetup, listOrders } from "@lemonsqueezy/lemonsqueezy.js";
import { env } from "wasp/server";
import type {
  CreateCheckoutSessionArgs,
  FetchCustomerPortalUrlArgs,
  PaymentProcessor,
} from "../paymentProcessor";
import { getPaymentProcessorPlanId } from "../paymentProcessorPlans";
import { createLemonSqueezyCheckoutSession } from "./checkoutUtils";
import { lemonSqueezyMiddlewareConfigFn, lemonSqueezyWebhook } from "./webhook";

lemonSqueezySetup({
  apiKey: env.LEMONSQUEEZY_API_KEY,
});

export const lemonSqueezyPaymentProcessor: PaymentProcessor = {
  id: "lemonsqueezy",
  createCheckoutSession: async ({
    userId,
    userEmail,
    paymentPlan,
  }: CreateCheckoutSessionArgs) => {
    if (!userId)
      throw new Error(
        "User ID needed to create Lemon Squeezy Checkout Session",
      );
    const session = await createLemonSqueezyCheckoutSession({
      storeId: env.LEMONSQUEEZY_STORE_ID,
      variantId: getPaymentProcessorPlanId(paymentPlan),
      userEmail,
      userId,
    });
    return { session };
  },
  fetchCustomerPortalUrl: async (args: FetchCustomerPortalUrlArgs) => {
    const user = await args.prismaUserDelegate.findUniqueOrThrow({
      where: {
        id: args.userId,
      },
      select: {
        lemonSqueezyCustomerPortalUrl: true,
      },
    });
    // Note that Lemon Squeezy assigns a unique URL to each user after the first successful payment.
    // This is handled in the Lemon Squeezy webhook.
    return user.lemonSqueezyCustomerPortalUrl;
  },
  webhook: lemonSqueezyWebhook,
  webhookMiddlewareConfigFn: lemonSqueezyMiddlewareConfigFn,
  fetchTotalRevenue: async () => {
    let totalRevenue = 0;
    let hasNextPage = true;
    let currentPage = 1;

    while (hasNextPage) {
      const { data: response } = await listOrders({
        filter: {
          storeId: env.LEMONSQUEEZY_STORE_ID,
        },
        page: {
          number: currentPage,
          size: 100,
        },
      });

      if (response?.data) {
        for (const order of response.data) {
          totalRevenue += order.attributes.total;
        }
      }

      hasNextPage = !response?.meta?.page.lastPage;
      currentPage++;
    }

    // Revenue is in cents so we convert to dollars (or your main currency unit)
    return totalRevenue / 100;
  },
};
