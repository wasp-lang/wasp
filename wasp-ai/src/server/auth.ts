import { GetUserFieldsFn } from "@wasp/types";

export const getGoogleUserFields: GetUserFieldsFn = async (_context, args) => {
  return {
    email: args.profile.emails[0].value,
    username: args.profile.emails[0].value,
  };
};

export const getGoogleAuthConfig = () => {
  return {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ["profile", "email"],
  };
};


export const getGitHubUserFields: GetUserFieldsFn = async (_context, args) => {
  // NOTE: if we don't want to access users' emails, we can use scope ["user:read"]
  // instead of ["user"] and access args.profile.username instead
  const username = args.profile.username;
  const email = args.profile.emails[0].value;
  return { email, username };
};

export function getGitHubAuthConfig() {
  return {
    clientID: process.env.GITHUB_CLIENT_ID, // look up from env or elsewhere
    clientSecret: process.env.GITHUB_CLIENT_SECRET, // look up from env or elsewhere
    scope: ["user"],
  };
}