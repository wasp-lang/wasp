
import type {
  _Task,
  AuthenticatedQueryDefinition,
  Payload,
} from '../../_types/index.js'

// PUBLIC API
export type GetMyTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAdminReport<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

