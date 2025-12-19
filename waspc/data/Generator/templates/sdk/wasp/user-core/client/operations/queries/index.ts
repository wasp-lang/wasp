{{={= =}=}}
import { type QueryFor, createQuery } from './core'
{=# queries =}
{=& operationTypeImportStmt =}
{=/ queries =}
{=# queries =}

// PUBLIC API
export const {= operationName =}: QueryFor<{= operationTypeName =}> = createQuery<{= operationTypeName =}>(
  '{= queryRoute =}',
  {=& entitiesArray =},
)
{=/ queries =}

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core'
