import { handleRejection } from '../../utils.js'

import MySpecialAction from '../../actions/MySpecialAction.js'

export default handleRejection(async (req, res) => {
  const args = req.body || {}

  const context = {
    user: req.user
  }
  const result = await MySpecialAction(args, context)
  res.json(result)
})

