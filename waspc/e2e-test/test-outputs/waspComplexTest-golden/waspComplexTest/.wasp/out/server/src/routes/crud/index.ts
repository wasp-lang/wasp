import express from 'express'
import auth from '../../core/auth.js'

import { tasks } from './tasks.js'

export const rootCrudRouter = express.Router()

rootCrudRouter.use('/tasks', auth, tasks)
