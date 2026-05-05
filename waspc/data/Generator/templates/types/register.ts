{{={= =}=}}
// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module "wasp/types" {
  interface Register {
{=# prismaSetupFn.isDefined =}
    prismaSetupFn: typeof {=& prismaSetupFn.dynamicImportExpression =}
{=/ prismaSetupFn.isDefined =}
{=# serverEnvSchema.isDefined =}
    serverEnvSchema: typeof {=& serverEnvSchema.dynamicImportExpression =}
{=/ serverEnvSchema.isDefined =}
{=# clientEnvSchema.isDefined =}
    clientEnvSchema: typeof {=& clientEnvSchema.dynamicImportExpression =}
{=/ clientEnvSchema.isDefined =}
{=# webSocketFn.isDefined =}
    webSocketFn: typeof {=& webSocketFn.dynamicImportExpression =}
{=/ webSocketFn.isDefined =}
{=# emailUserSignupFields.isDefined =}
    emailUserSignupFields: typeof {=& emailUserSignupFields.dynamicImportExpression =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
    usernameAndPasswordUserSignupFields: typeof {=& usernameAndPasswordUserSignupFields.dynamicImportExpression =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}
  }
}


