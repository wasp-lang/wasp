{{={= =}=}}
import { type QueryFor, createQuery } from "./core";
import {
{=# queries =}
  type {= registeredOperationTypeName =},
{=/ queries =}
} from "../../../server/operations/queries/index";
{=# queries =}

// PUBLIC API
export const {= operationName =}: QueryFor<{= registeredOperationTypeName =}> = createQuery<{= registeredOperationTypeName =}>(
  "{= queryRoute =}",
  {=& entitiesArray =},
)
{=/ queries =}

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core"
