import { createQuery } from '../../middleware/operations.js'
import getTodoItems from '../../queries/getTodoItems.js'

export default createQuery(getTodoItems)
