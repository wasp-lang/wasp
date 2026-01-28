import { createQuery } from '../../middleware/operations.js'
import getTasks from '../../queries/getTasks.js'

export default createQuery(getTasks)
