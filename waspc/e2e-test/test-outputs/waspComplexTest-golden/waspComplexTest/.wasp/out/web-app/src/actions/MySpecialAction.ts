import { createAction } from './core'
import { MySpecialAction } from '../../../server/src/actions/MySpecialAction'

const action = createAction<MySpecialAction>(
  'operations/my-special-action',
  ['"User"'],
)

export default action
