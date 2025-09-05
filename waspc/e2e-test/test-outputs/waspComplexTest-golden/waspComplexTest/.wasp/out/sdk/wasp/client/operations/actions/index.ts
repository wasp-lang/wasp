import { type ActionFor, createAction } from './core'
import { MySpecialAction_ext } from 'wasp/server/operations/actions'

// PUBLIC API
export const mySpecialAction: ActionFor<MySpecialAction_ext> = createAction<MySpecialAction_ext>(
  'operations/my-special-action',
  ['User'],
)
