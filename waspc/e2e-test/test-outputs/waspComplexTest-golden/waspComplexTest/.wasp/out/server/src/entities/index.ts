import {
  type User,
  type SocialLogin,
  type Task,
} from "@prisma/client"

export {
  type User,
  type SocialLogin,
  type Task,
} from "@prisma/client"

export type Entity = 
  | User
  | SocialLogin
  | Task
  | never

export type EntityName = 
  | "User"
  | "SocialLogin"
  | "Task"
  | never
