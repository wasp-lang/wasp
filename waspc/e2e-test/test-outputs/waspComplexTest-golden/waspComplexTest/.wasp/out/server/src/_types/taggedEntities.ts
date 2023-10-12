// Wasp internally uses the types defined in this file for typing entity maps in
// operation contexts.
//
// We must explicitly tag all entities with their name to avoid issues with
// structural typing. See https://github.com/wasp-lang/wasp/pull/982 for details.
import { 
  type Entity, 
  type EntityName,
  type User,
  type SocialLogin,
  type Task,
} from '../entities'

export type _User = WithName<User, "User">
export type _SocialLogin = WithName<SocialLogin, "SocialLogin">
export type _Task = WithName<Task, "Task">

export type _Entity = 
  | _User
  | _SocialLogin
  | _Task
  | never

type WithName<E extends Entity, Name extends EntityName> = 
  E & { _entityName: Name }
