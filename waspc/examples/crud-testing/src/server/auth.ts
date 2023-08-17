import { AdditionalSignupFieldsConfig } from "@wasp/types";

export function getAdditionalFields() {
  return {
    address: {
      get: (data): string => {
        return ensureString(data.address, "Address is required");
      },
      validate: (value) => {
        const address = ensureString(value, "Address is required");
        if (!address) {
          throw new Error("Address is required");
        }
        if (address.length < 5) {
          throw new Error("Address must be at least 5 characters long");
        }
      },
    },
  } satisfies AdditionalSignupFieldsConfig;
}

function ensureString(value: unknown, message: string): string {
  if (typeof value !== "string") {
    throw new Error(message);
  }
  return value;
}
