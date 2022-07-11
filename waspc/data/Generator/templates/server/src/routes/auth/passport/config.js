{{={= =}=}}

{=& onSignInJsFnImportStatement =}

export const passportRoutePrefix = '/external'
export const passportFullRoutePrefix = '/auth' + passportRoutePrefix

export const googleRoutePrefix = '/google'
export const googleFullRoutePrefix = passportFullRoutePrefix + googleRoutePrefix

export const googleLoginPath = '/login'
export const googleFullLoginPath = googleFullRoutePrefix + googleLoginPath

export const authConfig =  {
  onSignInFn: {= onSignInJsFnIdentifier =},
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}
