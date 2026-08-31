// PUBLIC API
export { google } from './providers/google.js';

// PUBLIC API
export { slack } from './providers/slack.js';

// PUBLIC API
export { discord } from './providers/discord.js';

// PUBLIC API
export { github } from './providers/github.js';
// PUBLIC API
export { microsoft } from './providers/microsoft.js';

// PUBLIC API
export { completeOAuthLogin } from './completeOAuthLogin.js'

// PRIVATE API
export {
  providerLoginPath,
  providerCallbackPath,
  getOAuthLoginErrorRedirectUrl,
  getOAuthLoginResultUrl,
} from './redirect.js'
