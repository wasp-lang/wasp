{{={= =}=}}
import type { QueryFor } from '@wasp.sh/lib-sdk-core/browser'
import { createQuery } from "./core";
import type {
{=# queries =}
  {= registeredOperationTypeName =},
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
