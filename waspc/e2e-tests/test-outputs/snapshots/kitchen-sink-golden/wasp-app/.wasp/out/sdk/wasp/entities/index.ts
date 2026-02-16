import {
  type User,
  type Task,
  type TaskVote,
  type UppercaseTextRequest,
} from "@prisma/client"

export {
  type User,
  type Task,
  type TaskVote,
  type UppercaseTextRequest,
  type Auth,
  type AuthIdentity,
} from "@prisma/client"

export type Entity = 
  | User
  | Task
  | TaskVote
  | UppercaseTextRequest
  | never

export type EntityName = 
  | "User"
  | "Task"
  | "TaskVote"
  | "UppercaseTextRequest"
  | never
