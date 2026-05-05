{{={= =}=}}
// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module 'wasp/types' {
  interface CrudOverridesRegister {
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
