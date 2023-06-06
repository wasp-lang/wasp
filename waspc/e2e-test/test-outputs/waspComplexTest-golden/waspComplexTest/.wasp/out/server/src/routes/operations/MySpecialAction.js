import { withOperationsMiddleware } from '../../middleware/operations.js'
import MySpecialAction from '../../actions/MySpecialAction.js'

export default withOperationsMiddleware(MySpecialAction)
