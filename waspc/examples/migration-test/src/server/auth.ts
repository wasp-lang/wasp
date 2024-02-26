import { defineUserSignupFields } from "wasp/server/auth";
import * as z from "zod";
import SecurePassword from "secure-password";
import { HttpError } from "wasp/server";
import {
  createProviderId,
  deserializeAndSanitizeProviderData,
  findAuthIdentity,
  updateAuthIdentityProviderData,
} from "wasp/server/auth";
import { MigratePassword } from "wasp/server/operations";

export const fields = defineUserSignupFields({
  address: (data) => {
    console.log("Received data:", data);
    const AddressSchema = z
      .string({
        required_error: "Address is required",
        invalid_type_error: "Address must be a string",
      })
      .min(10, "Address must be at least 10 characters long");
    const result = AddressSchema.safeParse(data.address);
    if (result.success === false) {
      throw new Error(result.error.issues[0].message);
    }
    return result.data;
  },
});

type MigratePasswordInput = {
  username: string;
  password: string;
};
type MigratePasswordOutput = {
  message: string;
};

export const migratePassword: MigratePassword<
  MigratePasswordInput,
  MigratePasswordOutput
> = async ({ password, username }, _context) => {
  const providerId = createProviderId("username", username);
  const authIdentity = await findAuthIdentity(providerId);

  if (!authIdentity) {
    throw new HttpError(400, "Something went wrong");
  }

  const providerData = deserializeAndSanitizeProviderData<"username">(
    authIdentity.providerData
  );

  try {
    const SP = new SecurePassword();

    // This will verify the password using the old algorithm
    const result = await SP.verify(
      Buffer.from(password),
      Buffer.from(providerData.hashedPassword, "base64")
    );

    if (result !== SecurePassword.VALID) {
      throw new HttpError(400, "Something went wrong");
    }

    // This will hash the password using the new algorithm and update the
    // provider data in the database.
    await updateAuthIdentityProviderData<"username">(providerId, providerData, {
      hashedPassword: password,
    });
  } catch (e) {
    throw new HttpError(400, "Something went wrong");
  }

  return {
    message: "Password migrated successfully.",
  };
};