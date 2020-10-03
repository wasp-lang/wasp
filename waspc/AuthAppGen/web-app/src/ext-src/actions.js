import HttpError from '../core/HttpError.js'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export const createUser = async (user, context) => {

  const newUser = await prisma.user.create({
    data: {
      email: user.email,
      password: user.password
    }
  })
}

