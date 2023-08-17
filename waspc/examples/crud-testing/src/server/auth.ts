import { defineAdditionalSignupFields } from "@wasp/types/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
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
});

function ensureString(value: unknown, message: string): string {
  if (typeof value !== "string") {
    throw new Error(message);
  }
  return value;
}
