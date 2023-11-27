{{={= =}=}}

import type { Prisma } from "@prisma/client"
import { contextWithUserEntity } from '../../utils.js'

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}

export type UserFieldsFromOAuthSignup = Prisma.{= userEntityName =}CreateInput

export type UserDefinedConfigFn = () => { [key: string]: any }

export type GetUserFieldsFn = (
    context: typeof contextWithUserEntity,
    args: { profile: { [key: string]: any } },
) => Promise<UserFieldsFromOAuthSignup>
