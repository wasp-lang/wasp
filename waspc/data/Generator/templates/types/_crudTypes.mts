{{={= =}=}}
declare module 'wasp/types' {
  interface CrudOverridesRegistry {
    '{= crud.name =}': {
{=# overrides.GetAll.isDefined =}
      GetAll: {=& overrides.GetAll.typeofImportExpression =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
      Get: {=& overrides.Get.typeofImportExpression =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
      Create: {=& overrides.Create.typeofImportExpression =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
      Update: {=& overrides.Update.typeofImportExpression =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
      Delete: {=& overrides.Delete.typeofImportExpression =}
{=/ overrides.Delete.isDefined =}
    }
  }
}
