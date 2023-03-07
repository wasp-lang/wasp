
import prisma from '../dbClient.js'

import { User } from '../entities'

export type FooBarContext = {
  user: User,
  entities: {
  }
}
