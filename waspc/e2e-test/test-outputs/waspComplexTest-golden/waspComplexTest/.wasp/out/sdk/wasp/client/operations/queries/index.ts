import { type QueryFor, createQuery } from './core'
import { MySpecialQuery_ext } from 'wasp/server/operations/queries'

// PUBLIC API
export const mySpecialQuery: QueryFor<MySpecialQuery_ext> = createQuery<MySpecialQuery_ext>(
  'operations/my-special-query',
  ['User'],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core'
