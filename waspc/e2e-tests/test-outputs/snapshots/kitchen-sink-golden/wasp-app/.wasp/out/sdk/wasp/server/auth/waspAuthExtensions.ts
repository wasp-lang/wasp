import { emailUserSignupFields as waspAuthExt_userSignupFieldsEmail } from 'virtual:wasp/user/features/auth/providers/email'
import { googleUserSignupFields as waspAuthExt_userSignupFieldsGoogle } from 'virtual:wasp/user/features/auth/providers/google'
import { gitHubUserSignupFields as waspAuthExt_userSignupFieldsGithub } from 'virtual:wasp/user/features/auth/providers/github'
import { slackUserSignupFields as waspAuthExt_userSignupFieldsSlack } from 'virtual:wasp/user/features/auth/providers/slack'
import { discordUserSignupFields as waspAuthExt_userSignupFieldsDiscord } from 'virtual:wasp/user/features/auth/providers/discord'
import { microsoftUserSignupFields as waspAuthExt_userSignupFieldsMicrosoft } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { googleConfig as waspAuthExt_configFnGoogle } from 'virtual:wasp/user/features/auth/providers/google'
import { gitHubConfig as waspAuthExt_configFnGithub } from 'virtual:wasp/user/features/auth/providers/github'
import { slackConfig as waspAuthExt_configFnSlack } from 'virtual:wasp/user/features/auth/providers/slack'
import { discordConfig as waspAuthExt_configFnDiscord } from 'virtual:wasp/user/features/auth/providers/discord'
import { microsoftConfig as waspAuthExt_configFnMicrosoft } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { getVerificationEmailContent as waspAuthExt_getVerificationEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { getPasswordResetEmailContent as waspAuthExt_getPasswordResetEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { onAfterEmailVerified as waspAuthExt_onAfterEmailVerified } from 'virtual:wasp/user/features/auth/hooks'

// PRIVATE API
/**
 * The user-authored functions Wasp's own auth calls back into, keyed the way
 * the `@wasp.sh/auth` lib expects them. Absent ones are `undefined`, and the
 * lib falls back to its defaults.
 */
export const waspAuthExtensions = {
  userSignupFields: {
    'username': undefined,
    'email': waspAuthExt_userSignupFieldsEmail,
    'google': waspAuthExt_userSignupFieldsGoogle,
    'github': waspAuthExt_userSignupFieldsGithub,
    'keycloak': undefined,
    'slack': waspAuthExt_userSignupFieldsSlack,
    'discord': waspAuthExt_userSignupFieldsDiscord,
    'microsoft': waspAuthExt_userSignupFieldsMicrosoft,
  },
  configFns: {
    'google': waspAuthExt_configFnGoogle,
    'github': waspAuthExt_configFnGithub,
    'keycloak': undefined,
    'slack': waspAuthExt_configFnSlack,
    'discord': waspAuthExt_configFnDiscord,
    'microsoft': waspAuthExt_configFnMicrosoft,
  },
  getVerificationEmailContent: waspAuthExt_getVerificationEmailContent,
  getPasswordResetEmailContent: waspAuthExt_getPasswordResetEmailContent,
  onAfterEmailVerified: waspAuthExt_onAfterEmailVerified,
  onBeforeOAuthRedirect: undefined,
}
