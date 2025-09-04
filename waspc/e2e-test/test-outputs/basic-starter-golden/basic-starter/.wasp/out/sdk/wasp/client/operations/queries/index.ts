import { type QueryFor, createQuery } from './core'
import { GetTasks_ext } from 'wasp/server/operations/queries'
import { GetTags_ext } from 'wasp/server/operations/queries'

// PUBLIC API
export const getTasks: QueryFor<GetTasks_ext> = createQuery<GetTasks_ext>(
  'operations/get-tasks',
  ['Task', 'Tag'],
)

// PUBLIC API
export const getTags: QueryFor<GetTags_ext> = createQuery<GetTags_ext>(
  'operations/get-tags',
  ['Tag'],
)

// PRIVATE API (used in SDK)
export { buildAndRegisterQuery } from './core'
