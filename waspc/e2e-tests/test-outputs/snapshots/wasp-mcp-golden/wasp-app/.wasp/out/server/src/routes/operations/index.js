import express from 'express'

import auth from 'wasp/server/core/auth'

import deleteTask from "./deleteTask.js"
import createTask from "./createTask.js"
import getTasks from "./getTasks.js"

const router = express.Router()

router.post('/delete-task', auth, deleteTask)
router.post('/create-task', auth, createTask)
router.post('/get-tasks', auth, getTasks)

export default router
