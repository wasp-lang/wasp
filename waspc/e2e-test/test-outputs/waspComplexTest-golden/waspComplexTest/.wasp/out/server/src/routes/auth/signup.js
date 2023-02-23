import prisma from '../../dbClient.js'
import { handleRejection, isPrismaError, prismaErrorToHttpError } from '../../utils.js'
import AuthError from '../../core/AuthError.js'
import HttpError from '../../core/HttpError.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}

  try {
    await prisma.user.create({ data: userFields })
  } catch (e) {
    if (e instanceof AuthError) {
      throw new HttpError(422, 'Validation failed', { message: e.message })
    } else if (isPrismaError(e)) {
      throw prismaErrorToHttpError(e)
    } else {
      throw new HttpError(500)
    }
  }

  res.send()
})
