import type {
  User,
  Task,
} from "@prisma/client"

export type {
  User,
  Task,
  Auth,
  AuthIdentity,
} from "@prisma/client"

export type Entity = 
  | User
  | Task
  | never

export type EntityName = 
  | "User"
  | "Task"
  | never
