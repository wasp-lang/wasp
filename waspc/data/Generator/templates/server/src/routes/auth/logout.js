import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  // Destroy the session.
  // This also empties the cookie set in the response.
  req.session = null

  return res.status(200).send()
})
