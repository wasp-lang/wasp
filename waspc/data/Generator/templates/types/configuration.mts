{{={= =}=}}
declare module 'wasp/types' {
  interface Registry {
{=# prismaSetupFn.isDefined =}
    userPrismaClient: ReturnType<{=& prismaSetupFn.typeofImportExpression =}>
{=/ prismaSetupFn.isDefined =}
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: {=& serverEnvSchema.typeofImportExpression =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: {=& clientEnvSchema.typeofImportExpression =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: {=& webSocketFn.typeofImportExpression =}
{=/ webSocketFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: {=& emailUserSignupFields.typeofImportExpression =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: {=& usernameAndPasswordUserSignupFields.typeofImportExpression =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}


