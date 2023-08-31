import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: {
    get: (data) => {
      return data.address as string | undefined;
    },
    validate: (address) => {
      if (!address) {
        throw new Error("Address is required");
      }
      if (address.length < 5) {
        throw new Error("Address must be at least 5 characters long");
      }
    },
  },
});
