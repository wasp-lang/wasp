import { defineUserSignupFields } from "wasp/server/auth";

export const fields = defineUserSignupFields({
  email: (data: any) => data.profile.email,
});

export function getConfig() {
  return {
    scopes: ["profile", "email"],
  };
}
