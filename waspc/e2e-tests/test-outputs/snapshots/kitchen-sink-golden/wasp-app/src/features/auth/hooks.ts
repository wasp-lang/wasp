import { type OnAfterEmailVerifiedHook } from "@wasp.sh/auth/server";
import { type User } from "wasp/entities";
import { HttpError, type PrismaClient } from "wasp/server";
import {
  type OnAfterLoginHook,
  type OnAfterSignupHook,
  type OnBeforeLoginHook,
  type OnBeforeSignupHook,
} from "wasp/server/auth";

export const onBeforeSignup: OnBeforeSignupHook = async ({ providerId }) => {
  if (providerId.providerUserId === "notallowed@email.com") {
    throw new HttpError(403, "On Before Signup Hook disallows this email.");
  }
};

export const onAfterSignup: OnAfterSignupHook = async ({ prisma, user }) => {
  await prisma.user.update({
    where: { id: user.id },
    data: {
      isOnAfterSignupHookCalled: true,
    },
  });
};

export const onAfterEmailVerified: OnAfterEmailVerifiedHook<
  User,
  PrismaClient
> = async ({ prisma, user }) => {
  await prisma.user.update({
    where: { id: user.id },
    data: {
      isOnAfterEmailVerifiedHookCalled: true,
      numTimesOnAfterEmailVerifiedCalled: {
        increment: 1,
      },
    },
  });
};

export const onBeforeLogin: OnBeforeLoginHook = async ({ providerId }) => {
  if (providerId.providerUserId === "cantlogin@email.com") {
    throw new HttpError(403, "On Before Login Hook disallows this email.");
  }
};

export const onAfterLogin: OnAfterLoginHook = async ({ prisma, user }) => {
  await prisma.user.update({
    where: { id: user.id },
    data: {
      isOnAfterLoginHookCalled: true,
    },
  });
};
