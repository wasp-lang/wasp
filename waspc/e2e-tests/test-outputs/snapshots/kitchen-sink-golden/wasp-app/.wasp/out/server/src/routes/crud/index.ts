import express from 'express'

import { tasks } from './tasks.js'
import { taskVotes } from './taskVotes.js'

export const rootCrudRouter = express.Router()

rootCrudRouter.use('/tasks', tasks)
rootCrudRouter.use('/taskVotes', taskVotes)
