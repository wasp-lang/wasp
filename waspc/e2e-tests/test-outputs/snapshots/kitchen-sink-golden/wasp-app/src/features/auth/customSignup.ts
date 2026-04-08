import {
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
} from "wasp/auth/validation";
import { prisma } from "wasp/server";
import {
  defineUserSignupFields,
  sanitizeAndSerializeProviderData,
} from "wasp/server/auth";
import { CustomSignup } from "wasp/server/operations";

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (typeof data.address !== "string") {
      throw new Error("Address is required.");
    }
    if (data.address.length < 10) {
      throw new Error("Address must be at least 10 characters long.");
    }
    return data.address;
  },
});

type CustomSignupInput = {
  email: string;
  password: string;
  address: string;
};
type CustomSignupOutput = {
  success: boolean;
  message: string;
};

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args) => {
  ensureValidEmail(args);
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);

  try {
    await prisma.auth.create({
      data: {
        user: {
          create: {
            address: args.address,
          },
        },
        identities: {
          create: {
            providerName: "email",
            providerUserId: args.email,
            providerData: await sanitizeAndSerializeProviderData<"email">({
              hashedPassword: args.password,
              isEmailVerified: true,
              emailVerificationSentAt: null,
              passwordResetSentAt: null,
            }),
          },
        },
      },
    });
  } catch (e: any) {
    return {
      success: false,
      message: e.message,
    };
  }

  return {
    success: true,
    message: "User created successfully",
  };
};
