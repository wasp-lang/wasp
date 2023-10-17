import type { GetUserFieldsFn } from "@wasp/types";

export const getUserFields: GetUserFieldsFn = async (_context, args) => {
  const email = args.profile.emails[0].value;
  return { email };
};

export function getConfig() {
  return {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ["profile", "email"],
  };
}
