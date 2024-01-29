{{={= =}=}}

import type { Prisma } from "@prisma/client"
import { contextWithUserEntity } from 'wasp/auth/utils'

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}

export type UserFieldsFromOAuthSignup = Prisma.{= userEntityName =}CreateInput

export type UserDefinedConfigFn = () => { [key: string]: any }
