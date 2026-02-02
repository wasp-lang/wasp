{{={= =}=}}
{=# enabledProviders.isGoogleAuthEnabled =}
// PUBLIC API
export { google } from './providers/google.js';
{=/ enabledProviders.isGoogleAuthEnabled =}

{=# enabledProviders.isSlackAuthEnabled =}
// PUBLIC API
export { slack } from './providers/slack.js';
{=/ enabledProviders.isSlackAuthEnabled =}

{=# enabledProviders.isDiscordAuthEnabled =}
// PUBLIC API
export { discord } from './providers/discord.js';
{=/ enabledProviders.isDiscordAuthEnabled =}

{=# enabledProviders.isGitHubAuthEnabled =}
// PUBLIC API
export { github } from './providers/github.js';
{=/ enabledProviders.isGitHubAuthEnabled =}
{=# enabledProviders.isKeycloakAuthEnabled =}
// PUBLIC API
export { keycloak } from './providers/keycloak.js';
{=/ enabledProviders.isKeycloakAuthEnabled =}
{=# enabledProviders.isMicrosoftEntraAuthEnabled =}
// PUBLIC API
export { microsoftEntra } from './providers/microsoftEntra.js';
{=/ enabledProviders.isMicrosoftEntraAuthEnabled =}

// PRIVATE API
export {
  loginPath,
  callbackPath,
  exchangeCodeForTokenPath,
  handleOAuthErrorAndGetRedirectUri,
  getRedirectUriForOneTimeCode,
} from './redirect.js'

// PRIVATE API
export {
  tokenStore,
} from './oneTimeCode.js'
