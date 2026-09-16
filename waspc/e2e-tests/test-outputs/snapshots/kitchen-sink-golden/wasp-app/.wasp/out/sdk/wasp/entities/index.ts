import type {
  User,
  Task,
  TaskVote,
  UppercaseTextRequest,
} from "@prisma/client"

export type {
  User,
  Task,
  TaskVote,
  UppercaseTextRequest,
  Auth,
  AuthIdentity,
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
