{{={= =}=}}
{=# extensions =}
{=# import.isDefined =}
{=& import.importStatement =}
{=/ import.isDefined =}
{=/ extensions =}

// PRIVATE API
/**
 * The user-authored functions Wasp's own auth calls back into, keyed the way
 * the `@wasp.sh/auth` lib expects them. Absent ones are `undefined`, and the
 * lib falls back to its defaults.
 */
export const waspAuthExtensions = {
  userSignupFields: {
    {=# userSignupFields =}
    '{= key =}': {=# import.isDefined =}{= import.importIdentifier =}{=/ import.isDefined =}{=^ import.isDefined =}undefined{=/ import.isDefined =},
    {=/ userSignupFields =}
  },
  configFns: {
    {=# configFns =}
    '{= key =}': {=# import.isDefined =}{= import.importIdentifier =}{=/ import.isDefined =}{=^ import.isDefined =}undefined{=/ import.isDefined =},
    {=/ configFns =}
  },
  {=# singles =}
  {= name =}: {=# import.isDefined =}{= import.importIdentifier =}{=/ import.isDefined =}{=^ import.isDefined =}undefined{=/ import.isDefined =},
  {=/ singles =}
}
