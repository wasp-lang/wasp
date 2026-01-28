import { defineUserSignupFields } from "wasp/server/auth";

export function config() {
  console.log("Inside user-supplied Discord config");
  return {
    scopes: ["identify", "email"],
  };
}

export const userSignupFields = defineUserSignupFields({});
