import express from 'express'

import { tasks } from './tasks.js'
import { moduleTodos } from './moduleTodos.js'

export const rootCrudRouter = express.Router()

rootCrudRouter.use('/tasks', tasks)
rootCrudRouter.use('/moduleTodos', moduleTodos)
