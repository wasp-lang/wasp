import { createQuery } from './core'
import { MySpecialQuery } from '../../../server/src/queries/MySpecialQuery'


const query = createQuery<MySpecialQuery>(
  'operations/my-special-query',
  ['"User"'],
)

export default query
