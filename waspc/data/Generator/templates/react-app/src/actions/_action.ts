{{={= =}=}}
import { createAction } from './core'
{=& operationTypeImportStmt =}

const action = createAction<{= operationTypeName =}>(
  '{= actionRoute =}',
  {=& entitiesArray =},
)

export default action
