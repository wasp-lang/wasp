{{={= =}=}}
import { createAction } from './core'
{=# actions =}
{=& operationTypeImportStmt =}
{=/ actions =}
{=# actions =}

export const {= operationName =} = createAction<{= operationTypeName =}>(
  '{= actionRoute =}',
  {=& entitiesArray =},
)
{=/ actions =}