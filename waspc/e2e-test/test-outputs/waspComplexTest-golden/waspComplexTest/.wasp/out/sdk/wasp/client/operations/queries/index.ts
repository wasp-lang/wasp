import { type QueryFor, createQuery } from './core'
import { MySpecialQuery } from 'wasp/server/operations/queries'

// PUBLIC API
export const mySpecialQuery: QueryFor<MySpecialQuery> = createQuery<MySpecialQuery>(
  'operations/my-special-query',
  ['User'],
)

// PRIVATE API
export { addMetadataToQuery } from './core'
