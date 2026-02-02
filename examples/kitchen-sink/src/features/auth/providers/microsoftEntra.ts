import { defineUserSignupFields } from "wasp/server/auth";

export function config() {
  return {
    scopes: ["openid", "profile", "email"],
  };
}

export const userSignupFields = defineUserSignupFields({});
