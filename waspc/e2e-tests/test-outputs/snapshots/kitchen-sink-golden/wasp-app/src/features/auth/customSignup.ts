import {
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
} from "wasp/auth/validation";
import {
  defineUserSignupFields,
  getIdentityStore,
  hashPassword,
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

export const customSignup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args) => {
  ensureValidEmail(args);
  ensurePasswordIsPresent(args);
  ensureValidPassword(args);

  try {
    // The same identity store Wasp's own signup flow uses -- no raw table
    // access needed. Hashing stays the caller's explicit job.
    await getIdentityStore("email").createIdentity(
      args.email,
      {
        data: {
          isEmailVerified: true,
          emailVerificationSentAt: null,
          passwordResetSentAt: null,
        },
        secrets: {
          hashedPassword: await hashPassword(args.password),
        },
      },
      { address: args.address },
    );
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
