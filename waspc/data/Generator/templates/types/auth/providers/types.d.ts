{{={= =}=}}
import type { InferUserSignupFields } from 'wasp/auth/providers/types'

{=# emailUserSignupFields.isDefined =}
{=& emailUserSignupFields.importStatement =}
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
{=& usernameAndPasswordUserSignupFields.importStatement =}
{=/ usernameAndPasswordUserSignupFields.isDefined =}

declare module 'wasp/auth/providers/types' {
{=# emailUserSignupFields.isDefined =}
  export type UserEmailSignupFieldsTypeTest = InferUserSignupFields<typeof {= emailUserSignupFields.importIdentifier =}>;
{=/ emailUserSignupFields.isDefined =}
{=# usernameAndPasswordUserSignupFields.isDefined =}
  export type UserUsernameAndPasswordSignupFieldsTypeTest = InferUserSignupFields<typeof {= usernameAndPasswordUserSignupFields.importIdentifier =}>;
{=/ usernameAndPasswordUserSignupFields.isDefined =}
}
