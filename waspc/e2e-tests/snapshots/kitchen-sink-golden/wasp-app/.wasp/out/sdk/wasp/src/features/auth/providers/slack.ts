import { defineUserSignupFields } from "wasp/server/auth";

export function config() {
  console.log("Inside user-supplied Slack config");
  return {
    scopes: ["openid", "email", "profile"],
  };
}

export const userSignupFields = defineUserSignupFields({});
