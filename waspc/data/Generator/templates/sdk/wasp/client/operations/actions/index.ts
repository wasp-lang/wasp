{{={= =}=}}
import { type ActionFor, createAction } from './core'
{=# actions =}
{=& operationTypeImportStmt =}
{=/ actions =}
{=# actions =}

// PUBLIC API
export const {= operationName =}: ActionFor<{= operationTypeName =}> = createAction<{= operationTypeName =}>(
  '{= actionRoute =}',
  {=& entitiesArray =},
)
{=/ actions =}