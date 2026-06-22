import { Checkout } from "@polar-sh/sdk/models/components/checkout.js";
import { Customer } from "@polar-sh/sdk/models/components/customer.js";
import { CHECKOUT_SUCCESS_URL } from "../paths";
import { polarClient } from "./polarClient";

/**
 * Returns a Polar customer for the given User email, creating a customer if none exist.
 *
 * NOTE: Polar enforces unique emails and `externalId`.
 *       Additionally, `externalId` can't be changed once set.
 */
export async function ensurePolarCustomer(
  userId: string,
  userEmail: string,
): Promise<Customer> {
  const polarCustomers = await polarClient.customers.list({
    email: userEmail,
  });

  if (polarCustomers.result.items.length === 0) {
    return polarClient.customers.create({
      externalId: userId,
      email: userEmail,
    });
  } else {
    return polarCustomers.result.items[0];
  }
}

interface CreatePolarCheckoutSessionArgs {
  productId: string;
  customerId: string;
}

export function createPolarCheckoutSession({
  productId,
  customerId,
}: CreatePolarCheckoutSessionArgs): Promise<Checkout> {
  return polarClient.checkouts.create({
    products: [productId],
    successUrl: CHECKOUT_SUCCESS_URL,
    customerId,
  });
}
