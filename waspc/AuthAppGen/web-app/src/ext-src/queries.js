import HttpError from '../core/HttpError.js'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export const getUsers = async (args, context) => {
  const users = await prisma.user.findMany({})

  return users
}

