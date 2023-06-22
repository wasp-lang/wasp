import {
  User,
  SocialLogin,
} from '@prisma/client'
  
export type {
  User,
  SocialLogin,
} from '@prisma/client'

export type Entity = 
  | User
  | SocialLogin
  | never
