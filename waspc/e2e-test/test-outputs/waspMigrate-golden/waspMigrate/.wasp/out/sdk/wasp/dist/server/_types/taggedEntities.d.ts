import { type Entity, type EntityName, type Task } from 'wasp/entities';
export type _Task = WithName<Task, "Task">;
export type _Entity = _Task | never;
type WithName<E extends Entity, Name extends EntityName> = E & {
    _entityName: Name;
};
export {};
