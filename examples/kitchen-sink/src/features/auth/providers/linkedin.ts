import { defineUserSignupFields } from "wasp/server/auth";

export function linkedinConfig() {
  return {
    scopes: ["profile", "email"],
  };
}

export const linkedinUserSignupFields = defineUserSignupFields({});
