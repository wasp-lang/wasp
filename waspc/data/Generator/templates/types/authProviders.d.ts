{{={= =}=}}
{=# emailUserSignupFields.isDefined =}
{=& emailUserSignupFields.importStatement =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
{=& usernameAndPasswordUserSignupFields.importStatement =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}

declare module 'wasp/types' {
  interface Register {
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {= emailUserSignupFields.importIdentifier =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {= usernameAndPasswordUserSignupFields.importIdentifier =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}
