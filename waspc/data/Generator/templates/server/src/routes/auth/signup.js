import { createNewUser } from '../../core/auth.js'
import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}

  await createNewUser(userFields)

  res.send()
})
