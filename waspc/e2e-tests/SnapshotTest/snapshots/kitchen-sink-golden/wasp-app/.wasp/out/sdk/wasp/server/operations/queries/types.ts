
import {
  type _Task,
  type _UppercaseTextRequest,
  type UnauthenticatedQueryDefinition,
  type AuthenticatedQueryDefinition,
  type Payload,
} from 'wasp/server/_types'

// PUBLIC API
export type GetTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetNumTasks<Input extends Payload = never, Output extends Payload = Payload> = 
  UnauthenticatedQueryDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetTask<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
      _Task,
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetSerializedObjects<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetTextUppercaseRequests<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
      _UppercaseTextRequest,
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetDate<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAnythingNoAuth<Input extends Payload = never, Output extends Payload = Payload> = 
  UnauthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAnythingAuth<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetTrueVoid<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAnyNoAuth<Input extends Payload = never, Output extends Payload = Payload> = 
  UnauthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAnyAuth<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

// PUBLIC API
export type GetAnyToNumberSpecified<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQueryDefinition<
    [
    ],
    Input,
    Output
  >

