import { defineUserSignupFields } from "wasp/server/auth";

export function discordConfig() {
  console.log("Inside user-supplied Discord config");
  return {
    scopes: ["identify", "email"],
  };
}

export const discordUserSignupFields = defineUserSignupFields({});
