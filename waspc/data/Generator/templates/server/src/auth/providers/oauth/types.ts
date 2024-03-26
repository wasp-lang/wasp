{{={= =}=}}

import type { Prisma } from "@prisma/client"

export type UserFieldsFromOAuthSignup = Prisma.{= userEntityName =}CreateInput

export type UserDefinedConfigFn = () => { [key: string]: any }
