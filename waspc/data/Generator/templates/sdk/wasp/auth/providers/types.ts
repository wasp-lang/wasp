{{={= =}=}}
import type { Prisma } from '@prisma/client'
import type { Expand, Exact } from '../../universal/types'

// PUBLIC API
export function defineUserSignupFields<T extends UserSignupFields>(
  fields: Exact<UserSignupFields, T>
): T {
  return fields
}

type UserEntityCreateInput = Prisma.{= userEntityUpper =}CreateInput

// PRIVATE API
export type PossibleUserFields = Expand<Partial<UserEntityCreateInput>>

// PRIVATE API
export type UserSignupFields = {
  [key in keyof PossibleUserFields]: FieldGetter<
    PossibleUserFields[key]
  >
}

type FieldGetter<T extends PossibleUserFieldValues> = (
  data: { [key: string]: unknown }
) => Promise<T | undefined> | T | undefined

type PossibleUserFieldValues = PossibleUserFields[keyof PossibleUserFields]
