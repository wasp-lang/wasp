{{={= =}=}}
import passport from 'passport'

{=# isGoogleAuthEnabled =}
import { setupGoogleAuth } from './google.js'
{=/ isGoogleAuthEnabled =}

{=& onSignInJsFnImportStatement =}

export function usePassport(app) {
  const authConfig =  {
    onSignInFn: {= onSignInJsFnIdentifier =},
    failureRedirectPath: "{= failureRedirectPath =}",
    successRedirectPath: "{= successRedirectPath =}",
  }

  {=# isGoogleAuthEnabled =}
  setupGoogleAuth(app, passport, authConfig)
  {=/ isGoogleAuthEnabled =}
}
