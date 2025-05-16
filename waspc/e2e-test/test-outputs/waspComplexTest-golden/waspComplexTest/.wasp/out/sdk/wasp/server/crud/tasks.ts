import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  _Task,
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "wasp/core/serialization";
import type {
  Task,
} from "wasp/entities";

type _WaspEntityTagged = _Task
type _WaspEntity = Task

/**
 * PUBLIC API
 */
export declare namespace tasks {
  export type GetAllQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>

  export type GetQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>

  export type CreateAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>


}

/**
 * PRIVATE API
 *
 * The types with the `Resolved` suffix are the types that are used internally by the Wasp client
 * to implement full-stack type safety.
 */
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
export type GetAllQueryResolved = tasks.GetAllQuery<GetAllInput, GetAllOutput>

type GetInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
export type GetQueryResolved = tasks.GetQuery<GetInput, GetOutput>

type CreateInput = SuperJSONObject & Prisma.XOR<
  Prisma.TaskCreateInput,
  Prisma.TaskUncheckedCreateInput
>
type CreateOutput = _WaspEntity
export type CreateActionResolved = tasks.CreateAction<CreateInput, CreateOutput>


