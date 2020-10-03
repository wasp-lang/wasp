import { handleRejection } from '../../utils.js'
import { createUser } from '../../ext-src/actions.js'

export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}
  const result = await createUser(args, context)
  res.json(result)
})

