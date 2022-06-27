{{={= =}=}}
import passport from 'passport'

import { setupGoogleAuth } from './google.js'

{=& onSignInJsFnImportStatement =}

export function usePassport(app) {
  const authConfig =  {
    onSignInFn: {= onSignInJsFnIdentifier =},
    failureRedirectPath: "{= failureRedirectPath =}",
    successRedirectPath: "{= successRedirectPath =}",
  }

  // TODO: Wrap conditionally.
  setupGoogleAuth(app, passport, authConfig)
}
