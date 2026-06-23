import type { OAuthProvider } from '@wasp.sh/lib-auth/browser'
import { slackSignInUrl } from './slack'
import { discordSignInUrl } from './discord'
import { googleSignInUrl } from './google'
import { githubSignInUrl } from './github'
import { microsoftSignInUrl } from './microsoft'

export const isEmailAuthEnabled = true as const

export const isUsernameAndPasswordAuthEnabled = false as const

export const passwordAuthIdentityField = 'email' as const

export const isPasswordAuthEnabled = passwordAuthIdentityField !== null

export const enabledOAuthProviders = [
  { id: 'slack', label: 'Slack', href: slackSignInUrl },
  { id: 'discord', label: 'Discord', href: discordSignInUrl },
  { id: 'google', label: 'Google', href: googleSignInUrl },
  { id: 'github', label: 'GitHub', href: githubSignInUrl },
  { id: 'microsoft', label: 'Microsoft', href: microsoftSignInUrl },
] satisfies OAuthProvider[]
