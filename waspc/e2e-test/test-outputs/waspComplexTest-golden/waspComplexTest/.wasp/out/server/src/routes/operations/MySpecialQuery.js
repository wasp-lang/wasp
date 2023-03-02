import { handleRejection } from '../../utils.js'

import MySpecialQuery from '../../queries/MySpecialQuery.js'

export default handleRejection(async (req, res) => {
  const args = req.body || {}

  const context = {
    user: req.user
  }

  const result = await MySpecialQuery(args, context)
  res.json(result)
})
