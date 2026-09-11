import { defineUserSignupFields } from "wasp/server/auth";

export function linkedInConfig() {
  return {
    scopes: ["profile", "email"],
  };
}

export const linkedInUserSignupFields = defineUserSignupFields({});
