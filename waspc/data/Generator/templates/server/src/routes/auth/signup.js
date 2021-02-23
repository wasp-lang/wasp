{{={= =}=}}
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}

  await prisma.{= userEntityLower =}.create({ data: userFields })

  res.send()
})
