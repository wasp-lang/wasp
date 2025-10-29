import { createAction } from '../../middleware/operations.js'
import deleteCompletedTasks from '../../actions/deleteCompletedTasks.js'

export default createAction(deleteCompletedTasks)
