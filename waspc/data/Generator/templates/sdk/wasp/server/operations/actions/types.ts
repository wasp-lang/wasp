{{={= =}=}}
{=! TODO: This template is exactly the same at the moment as one for query
          types, consider whether it makes sense to address this in the future. =}
import type {
  {=# allEntities =}
  {= internalTypeName =},
  {=/ allEntities =}
  {=# shouldImportNonAuthenticatedOperation =}
  UnauthenticatedActionDefinition,
  {=/ shouldImportNonAuthenticatedOperation =}
  {=# shouldImportAuthenticatedOperation =}
  AuthenticatedActionDefinition,
  {=/ shouldImportAuthenticatedOperation =}
  Payload,
} from '../../_types/index.js'

{=# operations =}
// PUBLIC API
export type {= typeName =}<Input extends Payload = never, Output extends Payload = Payload> = 
  {=# usesAuth =}
  AuthenticatedActionDefinition<
  {=/ usesAuth =}
  {=^ usesAuth =}
  UnauthenticatedActionDefinition<
  {=/ usesAuth =}
    [
    {=# entities =}
      {= internalTypeName =},
    {=/ entities =}
    ],
    Input,
    Output
  >

{=/ operations =}
