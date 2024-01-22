import { defineUserSignupFields } from "wasp/auth/providers/types";

export const googleUserSignupFields = defineUserSignupFields({
  address: (data) => "Placeholder address",
});
export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (typeof data.address !== "string") {
      throw new Error("Address must be provided on signup.");
    }
    return data.address;
  },
});
