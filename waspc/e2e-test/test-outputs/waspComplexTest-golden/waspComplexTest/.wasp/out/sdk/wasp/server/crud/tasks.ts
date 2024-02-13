import type {
  AuthenticatedAction,
  AuthenticatedQuery,
  _Task,
} from "wasp/server/_types";
import type { Prisma } from "@prisma/client";
import { Payload } from "wasp/server/_types/serialization";
import type {
  Task,
} from "wasp/entities";

type _WaspEntityTagged = _Task
type _WaspEntity = Task

/**
 * PUBLIC API
 */
export namespace tasks {
  export type GetAllQuery<Input extends Payload, Output extends Payload> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>

  export type GetQuery<Input extends Payload, Output extends Payload> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>

  export type CreateAction<Input extends Payload, Output extends Payload>= AuthenticatedAction<[_WaspEntityTagged], Input, Output>


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

type GetInput = Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
export type GetQueryResolved = tasks.GetQuery<GetInput, GetOutput>

type CreateInput = Prisma.TaskCreateInput
type CreateOutput = _WaspEntity
export type CreateActionResolved = tasks.CreateAction<CreateInput, CreateOutput>


