import {
  type _User,
  type AuthenticatedAction,
  type Payload,
} from '../_types'

export type MySpecialAction<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedAction<
    [
      _User,
    ],
    Input,
    Output
  >

