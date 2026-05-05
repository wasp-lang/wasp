{{={= =}=}}
import { type ActionFor, createAction } from './core'
{=# actions =}
{=& registeredOperationTypeImportStmt =}
{=/ actions =}
{=# actions =}

// PUBLIC API
export const {= operationName =}: ActionFor<{= registeredOperationTypeName =}> = createAction<{= registeredOperationTypeName =}>(
  '{= actionRoute =}',
  {=& entitiesArray =},
)
{=/ actions =}
