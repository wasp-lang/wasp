import {
  Task,
} from '@prisma/client'
  
export type {
  Task,
} from '@prisma/client'

export type Entity = 
  | Task
  | never
