import { AdditionalSignupFieldsConfig } from "@wasp/types";

export function getAdditionalFields() {
  return {
    address: {
      get: (data): string => {
        if (typeof data.address !== "string") {
          throw new Error("Address must be a string");
        }
        return data.address;
      },
      validate: (value) => {
        if (typeof value !== "string") {
          throw new Error("Address must be a string");
        }
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
