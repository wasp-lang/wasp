{{={= =}=}}
import { type QueryFor, createQuery } from './core'
{=# queries =}
{=& registeredOperationTypeImportStmt =}
{=/ queries =}
{=# queries =}

// PUBLIC API
export const {= operationName =}: QueryFor<{= registeredOperationTypeName =}> = createQuery<{= registeredOperationTypeName =}>(
  '{= queryRoute =}',
  {=& entitiesArray =},
)
{=/ queries =}

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core'
