{{={= =}=}}
import { type ActionFor, createAction } from "./core";
import {
{=# actions =}
  type {= registeredOperationTypeName =},
{=/ actions =}
} from "../../../server/operations/actions/index";
{=# actions =}

// PUBLIC API
export const {= operationName =}: ActionFor<{= registeredOperationTypeName =}> = createAction<{= registeredOperationTypeName =}>(
  "{= actionRoute =}",
  {=& entitiesArray =},
)
{=/ actions =}
