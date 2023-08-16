import { AdditionalSignupFieldsConfig } from "@wasp/types";

export function getAdditionalFields() {
  return {
    address: {
      get: (value) => {
        return value;
      },
      validate: (value) => {
        if (!value) {
          throw new Error("Address is required");
        }
        if (value.length < 5) {
          throw new Error("Address must be at least 5 characters long");
        }
      },
    },
  } satisfies AdditionalSignupFieldsConfig;
}
