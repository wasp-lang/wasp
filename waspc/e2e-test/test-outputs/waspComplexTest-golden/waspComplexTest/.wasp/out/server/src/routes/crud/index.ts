import express from 'express'

import { tasks } from './tasks.js'

export const rootCrudRouter = express.Router()

rootCrudRouter.use('/tasks', tasks)
