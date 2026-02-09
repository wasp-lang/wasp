import { defineUserSignupFields } from "wasp/server/auth";

export function config() {
  console.log("Inside user-supplied Microsoft config");
  return {
    scopes: ["openid", "profile", "email"],
  };
}

export const userSignupFields = defineUserSignupFields({});
