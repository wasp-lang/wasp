import { type Entity, type EntityName, type User, type SocialLogin, type Task } from 'wasp/entities';
export type _User = WithName<User, "User">;
export type _SocialLogin = WithName<SocialLogin, "SocialLogin">;
export type _Task = WithName<Task, "Task">;
export type _Entity = _User | _SocialLogin | _Task | never;
type WithName<E extends Entity, Name extends EntityName> = E & {
    _entityName: Name;
};
export {};
