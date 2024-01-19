import {
  type _Task,
  type AuthenticatedAction,
  type Payload,
} from 'wasp/server/_types'

export type CreateTask<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedAction<
    [
      _Task,
    ],
    Input,
    Output
  >

export type UpdateTask<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedAction<
    [
      _Task,
    ],
    Input,
    Output
  >

export type DeleteTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedAction<
    [
      _Task,
    ],
    Input,
    Output
  >

