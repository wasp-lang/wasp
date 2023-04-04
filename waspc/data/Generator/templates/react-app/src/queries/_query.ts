{{={= =}=}}
import { createQuery } from './core'
{=& operationTypeImportStmt =}


const query = createQuery<{= operationTypeName =}>(
  '{= queryRoute =}',
  {=& entitiesArray =},
)

export default query
