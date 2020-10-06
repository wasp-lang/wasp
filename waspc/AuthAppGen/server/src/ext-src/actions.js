import HttpError from '../core/HttpError.js'
import Prisma from '@prisma/client'

import { createNewUser } from '../auth.js'

const prisma = new Prisma.PrismaClient()

export const createUser = async (user, context) => {

  // TODO(matija): user here calls a function provided by wasp
  // instead of directly calling user.create(). That way we can hash password
  // and do whatever else is necessary.
  //
  // User will be able to import this function from e.g. @wasp/auth.
  const newUser = await createNewUser({ email: user.email, password: user.password })

  /*
  const newUser = await prisma.user.create({
    data: {
      email: user.email,
      password: user.password
    }
  })
  */
}

