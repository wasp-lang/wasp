import { GetUserFieldsFn } from "@wasp/types";

export const getUserFields: GetUserFieldsFn = async (_context, data) => {
  console.log(data);

  return {
    email: data.profile.emails[0].value,
  };
};

export const getGoogleConfig = () => {
  return {
    clientID: process.env["GOOGLE_CLIENT_ID"],
    clientSecret: process.env["GOOGLE_CLIENT_SECRET"],
    scope: ["profile", "email"],
  };
};
