
import type { Prisma } from "@prisma/client"
import { contextWithUserEntity } from '../../utils.js'

export type OAuthConfig = {
    clientID?: string;
    clientSecret?: string;
    scope?: string[];
}

export type UserFieldsFromOAuthSignup = Prisma.UserCreateInput

export type UserDefinedConfigFn = () => { [key: string]: any }
