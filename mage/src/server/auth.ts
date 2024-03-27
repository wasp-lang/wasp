import { defineUserSignupFields } from 'wasp/server/auth'

export const googleUserSignupFields = defineUserSignupFields({
  email: async (data: any) => data.profile.email,
  username: async (data: any) => data.profile?.email,
});

export const getGoogleAuthConfig = () => {
  return {
    scopes: ["profile", "email"],
  };
};

// NOTE: if we don't want to access users' emails, we can use scope ["user:read"]
// instead of ["user"] and access data.profile.username instead
export const gitHubUserSignupFields = defineUserSignupFields({
  email: async (data: any) => data.profile.email,
  username: async (data: any) => data.profile.login,
})

export function getGitHubAuthConfig() {
  return {
    scopes: ["user"],
  };
}
