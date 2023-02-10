import prisma from "../dbClient.js"
import { 
  type WaspEntity,
} from "../entities"

export type Query<Entities extends WaspEntity[], Input, Output> = Operation<Entities, Input, Output>

export type Action<Entities extends WaspEntity[], Input, Output> = Operation<Entities, Input, Output>


type Operation<Entities extends WaspEntity[], Input, Output> = (
  args: Input,
  context: {
    entities: EntityMap<Entities>,
  },
) => Promise<Output>

type PrismaDelegateFor<EntityName extends string> =
  never

type WaspNameFor<Entity extends WaspEntity> =
  never

type EntityMap<Entities extends WaspEntity[]> = {
  [EntityName in WaspNameFor<Entities[number]>]: PrismaDelegateFor<EntityName>
}
