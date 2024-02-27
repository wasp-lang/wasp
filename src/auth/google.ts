import { defineUserSignupFields } from 'wasp/server/auth'

export const fields = defineUserSignupFields({
  email: (data: any) => data.profile.emails[0].value
});

export function getConfig() {
  return {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ["profile", "email"],
  };
}
