{{={= =}=}}
declare module 'wasp/types' {
  interface CrudOverridesRegistry {
    '{= crud.name =}': {
{=# overrides.GetAll.isDefined =}
      GetAll: typeof {=& overrides.GetAll.dynamicImportExpr =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
      Get: typeof {=& overrides.Get.dynamicImportExpr =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
      Create: typeof {=& overrides.Create.dynamicImportExpr =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
      Update: typeof {=& overrides.Update.dynamicImportExpr =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
      Delete: typeof {=& overrides.Delete.dynamicImportExpr =}
{=/ overrides.Delete.isDefined =}
    }
  }
}
