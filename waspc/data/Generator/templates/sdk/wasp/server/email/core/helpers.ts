{{={= =}=}}
export { formatFromField } from '@wasp.sh/lib-sdk-core/node'

import type { EmailFromField } from "./types";

{=# isDefaultFromFieldDefined =}
// PRIVATE API
export function getDefaultFromField(): EmailFromField {
  return {
    email: {=& defaultFromField.email =},
    {=# defaultFromField.isNameDefined =}
    name: {=& defaultFromField.name =},
    {=/ defaultFromField.isNameDefined =}
  }
}
{=/ isDefaultFromFieldDefined =}
{=^ isDefaultFromFieldDefined =}
// PRIVATE API
export function getDefaultFromField(): EmailFromField {
  return {
    email: "",
    name: "",
  };
}
{=/ isDefaultFromFieldDefined =}
