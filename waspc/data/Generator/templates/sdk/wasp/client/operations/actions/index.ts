{{={= =}=}}
import { type ActionFor, createAction } from "./core";
import {
{=# actions =}
  type Registered{= genericOperationDefinitionTypeName =},
{=/ actions =}
} from "../../../server/operations/actions/index";
{=# actions =}

// PUBLIC API
export const {= operationName =}: ActionFor<Registered{= genericOperationDefinitionTypeName =}> = createAction<Registered{= genericOperationDefinitionTypeName =}>(
  "{= actionRoute =}",
  {=& entitiesArray =},
)
{=/ actions =}
