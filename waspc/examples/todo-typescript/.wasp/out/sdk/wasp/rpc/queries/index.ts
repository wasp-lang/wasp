import { createQuery } from './core'
import { GetTasks } from 'wasp/server/queries'

export const getTasks = createQuery<GetTasks>(
  'operations/get-tasks',
  ['Task'],
)

export { addMetadataToQuery } from './core'
