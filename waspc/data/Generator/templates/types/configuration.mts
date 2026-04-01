{{={= =}=}}
declare module 'wasp/types' {
  interface Registry {
{=# prismaSetupFn.isDefined =}
    userPrismaClient: typeof ReturnType<{=& prismaSetupFn.dynamicImportExpr =}>
{=/ prismaSetupFn.isDefined =}
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: typeof {=& serverEnvSchema.dynamicImportExpr =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: typeof {=& clientEnvSchema.dynamicImportExpr =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: typeof {=& webSocketFn.dynamicImportExpr =}
{=/ webSocketFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {=& emailUserSignupFields.dynamicImportExpr =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {=& usernameAndPasswordUserSignupFields.dynamicImportExpr =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}


