import { HttpError } from "wasp/server";
import * as z from "zod";
import { UnhandledWebhookEventError } from "../errors";

export async function parseWebhookPayload(rawPayload: string) {
  try {
    const rawEvent: unknown = JSON.parse(rawPayload);
    const { meta, data } = await genericEventSchema.parseAsync(rawEvent);
    switch (meta.event_name) {
      case "order_created": {
        const orderData = await orderDataSchema.parseAsync(data);
        return { eventName: meta.event_name, meta, data: orderData };
      }
      case "subscription_created":
      case "subscription_updated":
      case "subscription_cancelled":
      case "subscription_expired": {
        const subscriptionData = await subscriptionDataSchema.parseAsync(data);
        return { eventName: meta.event_name, meta, data: subscriptionData };
      }
      default:
        // If you'd like to handle more events, you can add more cases above.
        throw new UnhandledWebhookEventError(meta.event_name);
    }
  } catch (e: unknown) {
    if (e instanceof UnhandledWebhookEventError) {
      throw e;
    } else {
      console.error(e);
      throw new HttpError(400, "Error parsing Lemon Squeezy webhook payload");
    }
  }
}

export type SubscriptionData = z.infer<typeof subscriptionDataSchema>;

export type OrderData = z.infer<typeof orderDataSchema>;

/**
 * This schema is based on LemonSqueezyResponse type
 */
const genericEventSchema = z.object({
  meta: z.object({
    event_name: z.string(),
    custom_data: z.object({
      user_id: z.string(),
    }),
  }),
  data: z.unknown(),
});

/**
 * This schema is based on
 * @type import('@lemonsqueezy/lemonsqueezy.js').Order
 * specifically Order['data'].
 */
const orderDataSchema = z.object({
  attributes: z.object({
    customer_id: z.number(),
    status: z.string(),
    first_order_item: z.object({
      variant_id: z.number(),
    }),
    order_number: z.number(),
  }),
});

/**
 * This schema is based on
 * @type import('@lemonsqueezy/lemonsqueezy.js').Subscription
 * specifically Subscription['data'].
 */
const subscriptionDataSchema = z.object({
  attributes: z.object({
    customer_id: z.number(),
    status: z.string(),
    variant_id: z.number(),
  }),
});
