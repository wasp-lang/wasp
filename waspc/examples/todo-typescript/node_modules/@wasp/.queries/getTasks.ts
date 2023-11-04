import { createQuery } from './core'
import { GetTasks } from '../../../server/src/queries/getTasks'


const query = createQuery<GetTasks>(
  'operations/get-tasks',
  ['Task'],
)

export default query
