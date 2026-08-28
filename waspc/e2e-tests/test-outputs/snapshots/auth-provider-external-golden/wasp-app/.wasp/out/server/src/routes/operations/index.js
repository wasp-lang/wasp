import express from 'express'

import auth, { requireSessionProvider } from 'wasp/server/core/auth'

import createTask from './createTask.js'
import getMyTasks from './getMyTasks.js'

const router = express.Router()

router.post('/create-task', auth, createTask)
router.post('/get-my-tasks', auth, getMyTasks)

export default router
