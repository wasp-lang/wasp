{{={= =}=}}

{=& onSignInJsFnImportStatement =}

export const passportRoutePrefix = '/external'
export const googleRoutePrefix = '/google'
export const googleFullRoutePrefix = '/auth' + passportRoutePrefix + googleRoutePrefix

export const authConfig =  {
  onSignInFn: {= onSignInJsFnIdentifier =},
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}
