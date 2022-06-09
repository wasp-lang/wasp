import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  // Destroy the session.
  req.session = null

  return res.status(200).send()
})
