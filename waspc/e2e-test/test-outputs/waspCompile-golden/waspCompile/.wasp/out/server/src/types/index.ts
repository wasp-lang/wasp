import prisma from "../dbClient.js"
import { 
  type WaspEntity,
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
  never

type WaspNameFor<Entity extends WaspEntity> =
  never

type EntityMap<Entities extends WaspEntity[]> = {
  [EntityName in WaspNameFor<Entities[number]>]: PrismaDelegateFor<EntityName>
}
