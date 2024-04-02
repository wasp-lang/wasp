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

// PRIVATE API
export { addMetadataToQuery } from './core'
