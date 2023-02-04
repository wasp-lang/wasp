import prisma from "../dbClient.js"
import { 
  type WaspEntity,
  type Task,
} from "../entities"

export type Query<Entities extends WaspEntity[] = [], Input = unknown, Result = unknown> = Operation<Entities, Input, Result>

export type Action<Entities extends WaspEntity[] = [], Input = unknown, Result = unknown> = Operation<Entities, Input, Result>


type Operation<Entities extends WaspEntity[], Input, Result> = (
  args: Input,
  context: {
    entities: EntityMap<Entities>,
  },
) => Promise<Result>

type PrismaDelegateFor<EntityName extends string> =
  EntityName extends "Task" ? typeof prisma.task :
  never

type WaspNameFor<Entity extends WaspEntity> =
  Entity extends Task ? "Task" :
  never

type EntityMap<Entities extends WaspEntity[]> = {
  [EntityName in WaspNameFor<Entities[number]>]: PrismaDelegateFor<EntityName>
}
