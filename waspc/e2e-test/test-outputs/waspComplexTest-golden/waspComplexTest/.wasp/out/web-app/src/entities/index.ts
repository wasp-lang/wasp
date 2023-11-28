import {
  User,
  SocialLogin,
  Task,
} from '@prisma/client'
  
export type {
  User,
  SocialLogin,
  Task,
  Auth,
} from '@prisma/client'

export type Entity = 
  | User
  | SocialLogin
  | Task
  | never
