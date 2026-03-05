import { defineUserSignupFields } from "wasp/server/auth";

export function config() {
  console.log("Inside user-supplied GitHub config");
  return {
    scopes: ["user"],
  };
}

export const userSignupFields = defineUserSignupFields({});
