{{={= =}=}}
import { createQuery } from './core'
{=# queries =}
{=& operationTypeImportStmt =}
{=/ queries =}
{=# queries =}

export const {= operationName =} = createQuery<{= operationTypeName =}>(
  '{= queryRoute =}',
  {=& entitiesArray =},
)
{=/ queries =}

export { addMetadataToQuery } from './core'
