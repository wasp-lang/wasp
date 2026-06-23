{{={= =}=}}
import type { OAuthProvider } from '@wasp.sh/lib-auth/browser'
{=# isSlackAuthEnabled =}
import { slackSignInUrl } from './slack'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
import { discordSignInUrl } from './discord'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled =}
import { googleSignInUrl } from './google'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled =}
import { keycloakSignInUrl } from './keycloak'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
import { githubSignInUrl } from './github'
{=/ isGitHubAuthEnabled =}
{=# isMicrosoftAuthEnabled =}
import { microsoftSignInUrl } from './microsoft'
{=/ isMicrosoftAuthEnabled =}

export const isEmailAuthEnabled = {=# isEmailAuthEnabled =}true{=/ isEmailAuthEnabled =}{=^ isEmailAuthEnabled =}false{=/ isEmailAuthEnabled =} as const

export const isUsernameAndPasswordAuthEnabled = {=# isUsernameAndPasswordAuthEnabled =}true{=/ isUsernameAndPasswordAuthEnabled =}{=^ isUsernameAndPasswordAuthEnabled =}false{=/ isUsernameAndPasswordAuthEnabled =} as const

export const passwordAuthIdentityField = {=# isEmailAuthEnabled =}'email'{=/ isEmailAuthEnabled =}{=# isUsernameAndPasswordAuthEnabled =}'username'{=/ isUsernameAndPasswordAuthEnabled =}{=^ isAnyPasswordBasedAuthEnabled =}null{=/ isAnyPasswordBasedAuthEnabled =} as const

export const isPasswordAuthEnabled = passwordAuthIdentityField !== null

export const enabledOAuthProviders = [
  {=# isSlackAuthEnabled =}
  { id: 'slack', label: 'Slack', href: slackSignInUrl },
  {=/ isSlackAuthEnabled =}
  {=# isDiscordAuthEnabled =}
  { id: 'discord', label: 'Discord', href: discordSignInUrl },
  {=/ isDiscordAuthEnabled =}
  {=# isGoogleAuthEnabled =}
  { id: 'google', label: 'Google', href: googleSignInUrl },
  {=/ isGoogleAuthEnabled =}
  {=# isKeycloakAuthEnabled =}
  { id: 'keycloak', label: 'Keycloak', href: keycloakSignInUrl },
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
  { id: 'github', label: 'GitHub', href: githubSignInUrl },
  {=/ isGitHubAuthEnabled =}
  {=# isMicrosoftAuthEnabled =}
  { id: 'microsoft', label: 'Microsoft', href: microsoftSignInUrl },
  {=/ isMicrosoftAuthEnabled =}
] satisfies OAuthProvider[]
