import express from 'express'

import auth from 'wasp/core/auth'

import createTask from './createTask.js'
import updateTaskStatus from './updateTaskStatus.js'
import deleteCompletedTasks from './deleteCompletedTasks.js'
import createTag from './createTag.js'
import getTasks from './getTasks.js'
import getTags from './getTags.js'

const router = express.Router()

router.post('/create-task', auth, createTask)
router.post('/update-task-status', auth, updateTaskStatus)
router.post('/delete-completed-tasks', auth, deleteCompletedTasks)
router.post('/create-tag', auth, createTag)
router.post('/get-tasks', auth, getTasks)
router.post('/get-tags', auth, getTags)

export default router
