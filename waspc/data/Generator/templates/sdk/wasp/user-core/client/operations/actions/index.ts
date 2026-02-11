{{={= =}=}}
import { type ActionFor, createAction } from '../../../../core/client/operations/actions/core.js'
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
