
import {
  type _Task,
  type AuthenticatedQuery,
  type Payload,
} from 'wasp/server/_types'

export type GetTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQuery<
    [
      _Task,
    ],
    Input,
    Output
  >

