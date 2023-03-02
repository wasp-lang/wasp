import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  if (req.user) {
    return res.json(req.user)
  } else {
    return res.status(401).send()
  }
})
