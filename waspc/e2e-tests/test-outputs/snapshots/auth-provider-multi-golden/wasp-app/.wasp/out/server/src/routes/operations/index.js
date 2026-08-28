import express from 'express'

import auth, { requireSessionProvider } from 'wasp/server/core/auth'

import createTask from './createTask.js'
import getMyTasks from './getMyTasks.js'
import getAdminReport from './getAdminReport.js'

const router = express.Router()

router.post('/create-task', auth, createTask)
router.post('/get-my-tasks', auth, getMyTasks)
router.post('/get-admin-report', auth, requireSessionProvider(['wasp']), getAdminReport)

export default router
