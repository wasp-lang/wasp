{{={= =}=}}
import passport from 'passport'

import { setupGoogleAuth } from './google.js'

{=& onSignInJsFnImportStatement =}

export function usePassport(app) {
  app.use(passport.initialize())

  // TODO: Wrap conditionally.
  setupGoogleAuth(app, passport, {= onSignInJsFnIdentifier =})
}
