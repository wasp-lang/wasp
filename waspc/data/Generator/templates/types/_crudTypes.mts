{{={= =}=}}
declare module 'wasp/types' {
  interface CrudOverridesRegistry {
    '{= crud.name =}': {
{=# overrides.GetAll.isDefined =}
      GetAll: typeof {=& overrides.GetAll.typeofImportExpression =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
      Get: typeof {=& overrides.Get.typeofImportExpression =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
      Create: typeof {=& overrides.Create.typeofImportExpression =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
      Update: typeof {=& overrides.Update.typeofImportExpression =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
      Delete: typeof {=& overrides.Delete.typeofImportExpression =}
{=/ overrides.Delete.isDefined =}
    }
  }
}
