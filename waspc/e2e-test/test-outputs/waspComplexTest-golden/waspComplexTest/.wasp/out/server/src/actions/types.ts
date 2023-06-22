import {
  type _User,
  type AuthenticatedAction,
} from '../_types'

export type MySpecialAction<Input = never, Output = unknown> = 
  AuthenticatedAction<
    [
      _User,
    ],
    Input,
    Output
  >

