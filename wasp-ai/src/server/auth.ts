import { GetUserFieldsFn } from "@wasp/types";

export const getUserFields: GetUserFieldsFn = async (_context, args) => {
  return {
    email: args.profile.emails[0].value,
  };
};

export const getGoogleAuthConfig = () => {
  return {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ["profile", "email"],
  };
};
