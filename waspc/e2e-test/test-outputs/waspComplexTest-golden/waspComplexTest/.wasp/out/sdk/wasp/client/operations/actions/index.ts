import { type ActionFor, createAction } from './core'
import { MySpecialAction } from 'wasp/server/operations/actions'

// PUBLIC API
export const mySpecialAction: ActionFor<MySpecialAction> = createAction<MySpecialAction>(
  'operations/my-special-action',
  ['User'],
)
