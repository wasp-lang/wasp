
import type { Prisma } from "@prisma/client"
import { contextWithUserEntity } from '../../utils.js'

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}

export type CreateOAuthUser = Omit<Prisma.UserCreateInput, 'password'>

export type UserDefinedConfigFn = () => { [key: string]: any }

export type GetUserFieldsFn = (
    context: typeof contextWithUserEntity,
    args: { profile: { [key: string]: any } },
) => Promise<CreateOAuthUser>
