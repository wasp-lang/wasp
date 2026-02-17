{{={= =}=}}
{=# overrides.GetAll.isDefined =}
{=& overrides.GetAll.importStatement =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
{=& overrides.Get.importStatement =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
{=& overrides.Create.importStatement =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
{=& overrides.Update.importStatement =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
{=& overrides.Delete.importStatement =}
{=/ overrides.Delete.isDefined =}

declare module 'wasp/types' {
  interface Register {
{=# overrides.GetAll.isDefined =}
    'crud_{= crud.name =}_GetAll': typeof {= overrides.GetAll.importIdentifier =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
    'crud_{= crud.name =}_Get': typeof {= overrides.Get.importIdentifier =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
    'crud_{= crud.name =}_Create': typeof {= overrides.Create.importIdentifier =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
    'crud_{= crud.name =}_Update': typeof {= overrides.Update.importIdentifier =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
    'crud_{= crud.name =}_Delete': typeof {= overrides.Delete.importIdentifier =}
{=/ overrides.Delete.isDefined =}
  }
}
