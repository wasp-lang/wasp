import { User } from "wasp/entities";
import { PrismaClient } from "wasp/server";
import { PaymentPlanId, SubscriptionStatus } from "./plans";

export async function fetchUserPaymentProcessorUserId(
  userId: User["id"],
  prismaUserDelegate: PrismaClient["user"],
): Promise<string | null> {
  const user = await prismaUserDelegate.findUniqueOrThrow({
    where: {
      id: userId,
    },
    select: {
      paymentProcessorUserId: true,
    },
  });

  return user.paymentProcessorUserId;
}

interface UpdateUserPaymentProcessorUserIdArgs {
  userId: User["id"];
  paymentProcessorUserId: NonNullable<User["paymentProcessorUserId"]>;
}

export function updateUserPaymentProcessorUserId(
  { userId, paymentProcessorUserId }: UpdateUserPaymentProcessorUserIdArgs,
  prismaUserDelegate: PrismaClient["user"],
): Promise<User> {
  return prismaUserDelegate.update({
    where: {
      id: userId,
    },
    data: {
      paymentProcessorUserId,
    },
  });
}

interface UpdateUserSubscriptionArgs {
  paymentProcessorUserId: NonNullable<User["paymentProcessorUserId"]>;
  subscriptionStatus: SubscriptionStatus;
  paymentPlanId?: PaymentPlanId;
  datePaid?: Date;
}

export function updateUserSubscription(
  {
    paymentProcessorUserId,
    paymentPlanId,
    subscriptionStatus,
    datePaid,
  }: UpdateUserSubscriptionArgs,
  userDelegate: PrismaClient["user"],
): Promise<User> {
  return userDelegate.update({
    where: {
      paymentProcessorUserId,
    },
    data: {
      subscriptionPlan: paymentPlanId,
      subscriptionStatus,
      datePaid,
    },
  });
}

interface UpdateUserCreditsArgs {
  paymentProcessorUserId: NonNullable<User["paymentProcessorUserId"]>;
  numOfCreditsPurchased: number;
  datePaid: Date;
}

export function updateUserCredits(
  {
    paymentProcessorUserId,
    numOfCreditsPurchased,
    datePaid,
  }: UpdateUserCreditsArgs,
  userDelegate: PrismaClient["user"],
): Promise<User> {
  return userDelegate.update({
    where: {
      paymentProcessorUserId,
    },
    data: {
      credits: { increment: numOfCreditsPurchased },
      datePaid,
    },
  });
}
