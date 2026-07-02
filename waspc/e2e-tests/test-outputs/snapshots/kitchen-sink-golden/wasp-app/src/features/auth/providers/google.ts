import { defineUserSignupFields } from "wasp/server/auth";

export function googleConfig() {
  console.log("Inside user-supplied Google config");
  return {
    scopes: ["profile", "email"],
  };
}

export const googleUserSignupFields = defineUserSignupFields({});
