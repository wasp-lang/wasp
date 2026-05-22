import { defineUserSignupFields } from "wasp/server/auth";

export function microsoftConfig() {
  console.log("Inside user-supplied Microsoft config");
  return {
    scopes: ["openid", "profile", "email"],
  };
}

export const microsoftUserSignupFields = defineUserSignupFields({});
