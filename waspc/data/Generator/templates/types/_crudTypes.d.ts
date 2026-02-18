{{={= =}=}}
// Forces the file to be interpreted as a module.
// This is necessary for module augmentation to work.
export {}

declare module 'wasp/types' {
  interface RegisteredCrudOverrides {
    '{= crud.name =}': {
{=# overrides.GetAll.isDefined =}
      GetAll: typeof {=& overrides.GetAll.typeofImportExpr =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
      Get: typeof {=& overrides.Get.typeofImportExpr =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
      Create: typeof {=& overrides.Create.typeofImportExpr =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
      Update: typeof {=& overrides.Update.typeofImportExpr =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
      Delete: typeof {=& overrides.Delete.typeofImportExpr =}
{=/ overrides.Delete.isDefined =}
    }
  }
}
