import express from 'express'

import auth, { requireSchemes } from 'wasp/server/core/auth'

import createTask from './createTask.js'
import getMyTasks from './getMyTasks.js'
import getAdminReport from './getAdminReport.js'

const router = express.Router()

router.post('/create-task', auth, requireSchemes(['wasp', 'clerk']), createTask)
router.post('/get-my-tasks', auth, requireSchemes(['wasp', 'clerk']), getMyTasks)
router.post('/get-admin-report', auth, requireSchemes(['wasp']), getAdminReport)

export default router
