{{={= =}=}}
import { createQuery } from './core'
{=& operationTypeImportStmt =}


export default createQuery<{= operationTypeName =}>(
  '{= queryRoute =}',
  {=& entitiesArray =},
)
