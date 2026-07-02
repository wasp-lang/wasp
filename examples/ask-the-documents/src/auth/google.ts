import { defineUserSignupFields } from "wasp/server/auth";

export const googleUserSignupFields = defineUserSignupFields({
  email: (data: any) => data.profile.email,
});

export function getGoogleAuthConfig() {
  return {
    scopes: ["profile", "email"],
  };
}
