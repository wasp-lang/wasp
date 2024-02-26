import { defineUserSignupFields } from 'wasp/server/auth'

export const googleUserSignupFields = defineUserSignupFields({
  email: async (data) => (data.profile as any)?.emails?.[0]?.value,
  username: async (data) => (data.profile as any)?.emails?.[0]?.value,
});

export const getGoogleAuthConfig = () => {
  return {
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    scope: ["profile", "email"],
  };
};

// NOTE: if we don't want to access users' emails, we can use scope ["user:read"]
// instead of ["user"] and access data.profile.username instead
export const gitHubUserSignupFields = defineUserSignupFields({
  email: async (data) => (data.profile as any)?.emails?.[0]?.value,
  username: async (data) => (data.profile as any)?.username,
})

export function getGitHubAuthConfig() {
  return {
    clientID: process.env.GITHUB_CLIENT_ID, // look up from env or elsewhere
    clientSecret: process.env.GITHUB_CLIENT_SECRET, // look up from env or elsewhere
    scope: ["user"],
  };
}
