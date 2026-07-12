{{={= =}=}}
import { type QueryFor, createQuery } from "./core";
import {
{=# queries =}
  type Registered{= genericOperationDefinitionTypeName =},
{=/ queries =}
} from "../../../server/operations/queries/index";
{=# queries =}

// PUBLIC API
export const {= operationName =}: QueryFor<Registered{= genericOperationDefinitionTypeName =}> = createQuery<Registered{= genericOperationDefinitionTypeName =}>(
  "{= queryRoute =}",
  {=& entitiesArray =},
)
{=/ queries =}

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from "./core"
