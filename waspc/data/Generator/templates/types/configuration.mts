{{={= =}=}}
declare module 'wasp/types' {
  interface Registry {
{=# prismaSetupFn.isDefined =}
    userPrismaClient: ReturnType<typeof {=& prismaSetupFn.typeofImportExpr =}>
{=/ prismaSetupFn.isDefined =}
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: typeof {=& serverEnvSchema.typeofImportExpr =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: typeof {=& clientEnvSchema.typeofImportExpr =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: typeof {=& webSocketFn.typeofImportExpr =}
{=/ webSocketFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {=& emailUserSignupFields.typeofImportExpr =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {=& usernameAndPasswordUserSignupFields.typeofImportExpr =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}


