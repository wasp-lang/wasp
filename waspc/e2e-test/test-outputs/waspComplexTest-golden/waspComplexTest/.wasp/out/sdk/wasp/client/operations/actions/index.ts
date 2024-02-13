import { createAction } from './core'
import { MySpecialAction } from 'wasp/server/operations/actions'

// PUBLIC API
export const mySpecialAction = createAction<MySpecialAction>(
  'operations/my-special-action',
  ['User'],
)
