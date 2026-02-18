{{={= =}=}}
// Forces the file to be interpreted as a module.
// This is necessary for module augmentation to work.
export {}

declare module 'wasp/types' {
  interface RegisteredConfig {
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: typeof {=& serverEnvSchema.typeofImportExpr =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: typeof {=& clientEnvSchema.typeofImportExpr =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: typeof {=& webSocketFn.typeofImportExpr =}
{=/ webSocketFn.isDefined =}
{=# prismaSetupFn.isDefined =}
    prismaSetupFn: typeof {=& prismaSetupFn.typeofImportExpr =}
{=/ prismaSetupFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {=& emailUserSignupFields.typeofImportExpr =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {=& usernameAndPasswordUserSignupFields.typeofImportExpr =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}
