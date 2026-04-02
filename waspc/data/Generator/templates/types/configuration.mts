{{={= =}=}}
declare module 'wasp/types' {
  interface Registry {
{=# prismaSetupFn.isDefined =}
    prismaSetupFn: typeof {=& prismaSetupFn.typeDynamicImportExpression =}
{=/ prismaSetupFn.isDefined =}
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: typeof {=& serverEnvSchema.typeDynamicImportExpression =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: typeof {=& clientEnvSchema.typeDynamicImportExpression =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: typeof {=& webSocketFn.typeDynamicImportExpression =}
{=/ webSocketFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {=& emailUserSignupFields.typeDynamicImportExpression =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {=& usernameAndPasswordUserSignupFields.typeDynamicImportExpression =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}


