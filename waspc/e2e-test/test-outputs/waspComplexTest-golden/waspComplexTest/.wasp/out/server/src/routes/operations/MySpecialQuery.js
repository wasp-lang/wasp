import { withOperationsMiddleware } from '../../middleware/operations.js'
import MySpecialQuery from '../../queries/MySpecialQuery.js'

export default withOperationsMiddleware(MySpecialQuery)
