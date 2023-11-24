import { defineAdditionalSignupFields } from "@wasp/auth/providers/types.js";

export const additionalSignupFields = defineAdditionalSignupFields({
  email({ email }) {
    if (typeof email !== "string") {
      throw new Error(`Email must be a string`);
    }
    return email;
  },
});
