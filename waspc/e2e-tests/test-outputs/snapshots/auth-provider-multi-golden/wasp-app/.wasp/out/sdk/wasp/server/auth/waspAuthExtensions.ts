
// PRIVATE API
/**
 * The user-authored functions Wasp's own auth calls back into, keyed the way
 * the `@wasp.sh/auth` lib expects them. Absent ones are `undefined`, and the
 * lib falls back to its defaults.
 */
export const waspAuthExtensions = {
  userSignupFields: {
    'username': undefined,
    'email': undefined,
    'google': undefined,
    'github': undefined,
    'keycloak': undefined,
    'slack': undefined,
    'discord': undefined,
    'microsoft': undefined,
  },
  configFns: {
    'google': undefined,
    'github': undefined,
    'keycloak': undefined,
    'slack': undefined,
    'discord': undefined,
    'microsoft': undefined,
  },
  getVerificationEmailContent: undefined,
  getPasswordResetEmailContent: undefined,
  onAfterEmailVerified: undefined,
  onBeforeOAuthRedirect: undefined,
}
