import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  res.json(req.user)
})
