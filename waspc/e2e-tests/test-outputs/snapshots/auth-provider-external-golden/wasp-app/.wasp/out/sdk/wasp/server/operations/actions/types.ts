import {
  type _Task,
  type AuthenticatedActionDefinition,
  type Payload,
} from '../../_types/index.js'

// PUBLIC API
export type CreateTask<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedActionDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

