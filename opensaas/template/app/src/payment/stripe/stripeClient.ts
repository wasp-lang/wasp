import Stripe from "stripe";
import { env } from "wasp/server";

/**
 * The Stripe client API version.
 *
 * By default, Stripe uses the API version set in your Dashboard.
 *
 * You can override this by setting `apiVersion` when creating the Stripe client.
 * This is useful for testing new API versions before updating the
 * default version in your Dashboard.
 *
 * The Stripe Node SDK works with multiple API versions and follows semantic versioning.
 * Major SDK versions typically correspond to Stripe's biannual releases (like 'basil').
 * Each SDK version uses the API version that was current when it was released.
 *
 * Note: '2025-04-30.basil' follows Stripe's newer versioning format where:
 * - The date represents the release date
 * - The suffix ('basil') indicates the major release name
 *
 * Monthly API updates use the same suffix as the last major release but with updated dates.
 *
 * @see https://docs.stripe.com/api/versioning
 * @see https://docs.stripe.com/sdks/versioning
 */
const STRIPE_API_VERSION = "2025-04-30.basil";

export const stripeClient = new Stripe(env.STRIPE_API_KEY, {
  apiVersion: STRIPE_API_VERSION,
});
