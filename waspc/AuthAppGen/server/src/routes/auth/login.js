import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}

  // TODO(matija): verify email/pass and issue a token.

  res.json({})
})
