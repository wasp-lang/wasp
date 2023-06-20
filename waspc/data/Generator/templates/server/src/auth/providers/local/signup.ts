{{={= =}=}}
import { handleRejection } from '../../../utils.js'
import { createUser } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}

  await createUser({
    username: userFields.username,
    password: userFields.password,
  })

  return res.json({ success: true })
})
