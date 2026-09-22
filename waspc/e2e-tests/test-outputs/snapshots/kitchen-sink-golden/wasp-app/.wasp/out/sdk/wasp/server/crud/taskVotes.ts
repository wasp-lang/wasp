import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  _TaskVote,
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "../../core/serialization/index";
import type {
  TaskVote,
} from "wasp/entities";
import type { FromRegisterPath } from '@wasp.sh/lib-sdk-core'

type _WaspEntityTagged = _TaskVote
type _WaspEntity = TaskVote

/**
 * PUBLIC API
 */
export declare namespace taskVotes {
  export type GetAllQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>
}

/**
 * PRIVATE API
 */
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
export type RegisteredGetAllQuery = FromRegisterPath<['crudOverrides', 'taskVotes', 'GetAll'], taskVotes.GetAllQuery<GetAllInput, GetAllOutput>>
