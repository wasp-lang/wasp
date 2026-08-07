{{={= =}=}}
import { type ActionFor, createAction } from "./core";
import type {
{=# actions =}
  {= registeredOperationTypeName =},
{=/ actions =}
} from "../../../server/operations/actions/index";
{=# actions =}

// PUBLIC API
export const {= operationName =}: ActionFor<{= registeredOperationTypeName =}> = createAction<{= registeredOperationTypeName =}>(
  "{= actionRoute =}",
  {=& entitiesArray =},
)
{=/ actions =}
