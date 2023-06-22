
import {
  type _User,
  type AuthenticatedQuery,
} from '../_types'

export type MySpecialQuery<Input = never, Output = unknown> = 
  AuthenticatedQuery<
    [
      _User,
    ],
    Input,
    Output
  >

