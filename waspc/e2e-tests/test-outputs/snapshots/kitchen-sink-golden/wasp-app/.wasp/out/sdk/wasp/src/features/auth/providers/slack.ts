import { defineUserSignupFields } from "wasp/server/auth";

export function slackConfig() {
  console.log("Inside user-supplied Slack config");
  return {
    scopes: ["openid", "email", "profile"],
  };
}

export const slackUserSignupFields = defineUserSignupFields({});
