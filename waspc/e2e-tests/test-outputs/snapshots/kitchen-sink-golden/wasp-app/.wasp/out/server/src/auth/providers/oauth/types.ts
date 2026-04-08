
import type { Prisma } from "@prisma/client"

export type UserFieldsFromOAuthSignup = Prisma.UserCreateInput

export type UserDefinedConfigFn = () => { [key: string]: any }
