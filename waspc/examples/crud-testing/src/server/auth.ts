import { defineAdditionalSignupFields } from "@wasp/auth/index.js";

export const fields = defineAdditionalSignupFields({
  address: async (data) => {
    const address = data.address;
    await waitOneSecond();
    if (typeof address !== "string") {
      throw new Error("Address is required");
    }
    if (address.length < 5) {
      throw new Error("Address must be at least 5 characters long");
    }
    return address;
  },
});

const waitOneSecond = () => new Promise((resolve) => setTimeout(resolve, 1000));