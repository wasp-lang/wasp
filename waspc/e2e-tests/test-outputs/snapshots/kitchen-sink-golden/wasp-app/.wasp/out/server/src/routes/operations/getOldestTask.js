import { createQuery } from '../../middleware/operations.js'
import getOldestTask from '../../queries/getOldestTask.js'

export default createQuery(getOldestTask)
