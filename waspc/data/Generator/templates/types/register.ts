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
    operations: {
{=# operations =}
      '{= operationName =}': typeof {=& jsFn.dynamicImportExpression =}
    {=/ operations =}
    }
    crudOverrides: {
{=# cruds =}
      '{= name =}': {
{=# overrides.GetAll.isDefined =}
        GetAll: typeof {=& overrides.GetAll.dynamicImportExpression =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
        Get: typeof {=& overrides.Get.dynamicImportExpression =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
        Create: typeof {=& overrides.Create.dynamicImportExpression =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
        Update: typeof {=& overrides.Update.dynamicImportExpression =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
        Delete: typeof {=& overrides.Delete.dynamicImportExpression =}
{=/ overrides.Delete.isDefined =}
      }
{=/ cruds =}
    }
  }
}
