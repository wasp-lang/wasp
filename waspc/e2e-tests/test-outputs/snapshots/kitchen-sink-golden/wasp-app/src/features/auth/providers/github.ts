import { defineUserSignupFields } from "wasp/server/auth";

export function gitHubConfig() {
  console.log("Inside user-supplied GitHub config");
  return {
    scopes: ["user"],
  };
}

export const gitHubUserSignupFields = defineUserSignupFields({});
