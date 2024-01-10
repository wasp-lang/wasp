import {
  type User,
  type Task,
} from "@prisma/client"

export {
  type User,
  type Task,
  type Auth,
  type AuthIdentity,
} from "@prisma/client"

export type Entity = 
  | User
  | Task
  | never

export type EntityName = 
  | "User"
  | "Task"
  | never
