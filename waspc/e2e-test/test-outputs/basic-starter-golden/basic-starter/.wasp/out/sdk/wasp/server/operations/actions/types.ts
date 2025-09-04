import {
  type _Task,
  type _Tag,
  type AuthenticatedActionDefinition,
  type Payload,
} from 'wasp/server/_types'

// PUBLIC API
export type CreateTask<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedActionDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type UpdateTaskStatus<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedActionDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type DeleteCompletedTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedActionDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type CreateTag<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedActionDefinition<
    [
      _Tag,
    ],
    Input,
    Output
  >

