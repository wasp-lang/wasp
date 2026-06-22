import { HttpError } from "wasp/server";
import type {
  GenerateCheckoutSession,
  GetCustomerPortalUrl,
} from "wasp/server/operations";
import * as z from "zod";
import { PaymentPlanId, paymentPlans } from "../payment/plans";
import { ensureArgsSchemaOrThrowHttpError } from "../server/validation";
import { paymentProcessor } from "./paymentProcessor";

export type CheckoutSession = {
  sessionUrl: string | null;
  sessionId: string;
};

const generateCheckoutSessionSchema = z.nativeEnum(PaymentPlanId);

type GenerateCheckoutSessionInput = z.infer<
  typeof generateCheckoutSessionSchema
>;

export const generateCheckoutSession: GenerateCheckoutSession<
  GenerateCheckoutSessionInput,
  CheckoutSession
> = async (rawPaymentPlanId, context) => {
  if (!context.user) {
    throw new HttpError(
      401,
      "Only authenticated users are allowed to perform this operation",
    );
  }

  const paymentPlanId = ensureArgsSchemaOrThrowHttpError(
    generateCheckoutSessionSchema,
    rawPaymentPlanId,
  );
  const userId = context.user.id;
  const userEmail = context.user.email;
  if (!userEmail) {
    // If using the usernameAndPassword Auth method, switch to an Auth method that provides an email.
    throw new HttpError(403, "User needs an email to make a payment.");
  }

  const paymentPlan = paymentPlans[paymentPlanId];
  const { session } = await paymentProcessor.createCheckoutSession({
    userId,
    userEmail,
    paymentPlan,
    prismaUserDelegate: context.entities.User,
  });

  return {
    sessionUrl: session.url,
    sessionId: session.id,
  };
};

export const getCustomerPortalUrl: GetCustomerPortalUrl<
  void,
  string | null
> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(
      401,
      "Only authenticated users are allowed to perform this operation",
    );
  }

  return paymentProcessor.fetchCustomerPortalUrl({
    userId: context.user.id,
    prismaUserDelegate: context.entities.User,
  });
};
