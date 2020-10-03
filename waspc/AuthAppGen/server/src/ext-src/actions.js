import HttpError from '../core/HttpError.js'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export const createUser = async (user, context) => {

  // TODO(matija): user will here call a function provided by wasp
  // instead of directly calling user.create(). That way we can hash password
  // and do whatever else is necessary (e.g. generate token maybe).
  //
  // User will be able to import this function from e.g. @wasp/auth.
  const newUser = await prisma.user.create({
    data: {
      email: user.email,
      password: user.password
    }
  })
}

