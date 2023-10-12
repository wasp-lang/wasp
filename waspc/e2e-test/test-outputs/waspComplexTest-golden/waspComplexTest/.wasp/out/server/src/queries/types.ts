
import {
  type _User,
  type AuthenticatedQuery,
  type Payload,
} from '../_types'

export type MySpecialQuery<Input extends Payload = never, Output extends Payload = Payload> = 
  AuthenticatedQuery<
    [
      _User,
    ],
    Input,
    Output
  >

