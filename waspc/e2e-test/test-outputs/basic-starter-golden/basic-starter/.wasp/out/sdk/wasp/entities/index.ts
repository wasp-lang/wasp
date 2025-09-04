import {
  type User,
  type Task,
  type Tag,
} from "@prisma/client"

export {
  type User,
  type Task,
  type Tag,
  type Auth,
  type AuthIdentity,
} from "@prisma/client"

export type Entity = 
  | User
  | Task
  | Tag
  | never

export type EntityName = 
  | "User"
  | "Task"
  | "Tag"
  | never
