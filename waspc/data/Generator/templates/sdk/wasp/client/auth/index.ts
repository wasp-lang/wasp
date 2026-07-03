{{={= =}=}}
{=!
// These re-exports must come before the './ui' export: user code referenced
// by `auth.onAuthSucceededRedirect` imports the helpers from this module and
// is itself imported by the auth forms under './ui', creating an import
// cycle. Re-exporting the helpers first makes sure they are initialized
// before the cycle re-enters this module.
=}
export {
  redirectToFixed,
  redirectToOriginalRoute,
  type OnAuthSucceededRedirectContext,
  type OnAuthSucceededRedirectFn,
} from '@wasp.sh/lib-auth/browser'
export * from './ui'
{=# isEmailAuthEnabled =}
export * from './email'
{=/ isEmailAuthEnabled =}
{=# isUsernameAndPasswordAuthEnabled =}
export * from './username'
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isSlackAuthEnabled =}
export * from './slack'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
export * from './discord'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled  =}
export * from './google'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled  =}
export * from './keycloak'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
export * from './github'
{=/ isGitHubAuthEnabled =}
{=# isMicrosoftAuthEnabled =}
export * from './microsoft'
{=/ isMicrosoftAuthEnabled =}
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'
