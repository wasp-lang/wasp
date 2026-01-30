import {
  type Task,
} from "@prisma/client"

export {
  type Task,
} from "@prisma/client"

export type Entity = 
  | Task
  | never

export type EntityName = 
  | "Task"
  | never
