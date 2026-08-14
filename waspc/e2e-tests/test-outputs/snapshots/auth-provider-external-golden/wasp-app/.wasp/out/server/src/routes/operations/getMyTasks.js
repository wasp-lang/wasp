import { createQuery } from '../../middleware/operations.js'
import getMyTasks from '../../queries/getMyTasks.js'

export default createQuery(getMyTasks)
