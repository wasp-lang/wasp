{{={= =}=}}
// The import ensures the module is always loaded into the bundle.
// Otherwise, the module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module 'wasp/types' {
  interface CrudOverridesRegistry {
{=# cruds =}
    '{= name =}': {
{=# overrides.GetAll.isDefined =}
      GetAll: typeof {=& overrides.GetAll.typeDynamicImportExpression =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
      Get: typeof {=& overrides.Get.typeDynamicImportExpression =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
      Create: typeof {=& overrides.Create.typeDynamicImportExpression =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
      Update: typeof {=& overrides.Update.typeDynamicImportExpression =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
      Delete: typeof {=& overrides.Delete.typeDynamicImportExpression =}
{=/ overrides.Delete.isDefined =}
    }
{=/ cruds =}
  }
}
