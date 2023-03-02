import {
  type User,
  type SocialLogin,
} from "@prisma/client"

export {
  type User,
  type SocialLogin,
} from "@prisma/client"

export type Entity = 
  | User
  | SocialLogin
  | never

export type EntityName = 
  | "User"
  | "SocialLogin"
  | never
