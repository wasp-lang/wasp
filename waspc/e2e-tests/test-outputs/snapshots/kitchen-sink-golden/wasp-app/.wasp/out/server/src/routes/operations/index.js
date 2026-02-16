import express from 'express'

import auth from 'wasp/core/auth'

import customSignup from './customSignup.js'
import createTask from './createTask.js'
import updateTaskIsDone from './updateTaskIsDone.js'
import deleteCompletedTasks from './deleteCompletedTasks.js'
import toggleAllTasks from './toggleAllTasks.js'
import requestUppercaseText from './requestUppercaseText.js'
import testingAction from './testingAction.js'
import taskToTaskUnspecified from './taskToTaskUnspecified.js'
import taskToTaskSatisfies from './taskToTaskSatisfies.js'
import taskToTaskSpecified from './taskToTaskSpecified.js'
import voidToStringAuth from './voidToStringAuth.js'
import voidToStringNoAuth from './voidToStringNoAuth.js'
import unspecifiedToNumber from './unspecifiedToNumber.js'
import boolToStringAuth from './boolToStringAuth.js'
import boolToStringNoAuth from './boolToStringNoAuth.js'
import boolToVoidNoAuth from './boolToVoidNoAuth.js'
import boolToVoidAuth from './boolToVoidAuth.js'
import jsActionWithArgs from './jsActionWithArgs.js'
import getTasks from './getTasks.js'
import getNumTasks from './getNumTasks.js'
import getTask from './getTask.js'
import getSerializedObjects from './getSerializedObjects.js'
import getTextUppercaseRequests from './getTextUppercaseRequests.js'
import getDate from './getDate.js'
import getAnythingNoAuth from './getAnythingNoAuth.js'
import getAnythingAuth from './getAnythingAuth.js'
import getTrueVoid from './getTrueVoid.js'
import getAnyNoAuth from './getAnyNoAuth.js'
import getAnyAuth from './getAnyAuth.js'
import getAnyToNumberSpecified from './getAnyToNumberSpecified.js'

const router = express.Router()

router.post('/custom-signup', auth, customSignup)
router.post('/create-task', auth, createTask)
router.post('/update-task-is-done', auth, updateTaskIsDone)
router.post('/delete-completed-tasks', auth, deleteCompletedTasks)
router.post('/toggle-all-tasks', auth, toggleAllTasks)
router.post('/request-uppercase-text', auth, requestUppercaseText)
router.post('/testing-action', auth, testingAction)
router.post('/task-to-task-unspecified', auth, taskToTaskUnspecified)
router.post('/task-to-task-satisfies', auth, taskToTaskSatisfies)
router.post('/task-to-task-specified', auth, taskToTaskSpecified)
router.post('/void-to-string-auth', auth, voidToStringAuth)
router.post('/void-to-string-no-auth', voidToStringNoAuth)
router.post('/unspecified-to-number', auth, unspecifiedToNumber)
router.post('/bool-to-string-auth', auth, boolToStringAuth)
router.post('/bool-to-string-no-auth', boolToStringNoAuth)
router.post('/bool-to-void-no-auth', boolToVoidNoAuth)
router.post('/bool-to-void-auth', auth, boolToVoidAuth)
router.post('/js-action-with-args', auth, jsActionWithArgs)
router.post('/get-tasks', auth, getTasks)
router.post('/get-num-tasks', getNumTasks)
router.post('/get-task', auth, getTask)
router.post('/get-serialized-objects', auth, getSerializedObjects)
router.post('/get-text-uppercase-requests', auth, getTextUppercaseRequests)
router.post('/get-date', auth, getDate)
router.post('/get-anything-no-auth', getAnythingNoAuth)
router.post('/get-anything-auth', auth, getAnythingAuth)
router.post('/get-true-void', auth, getTrueVoid)
router.post('/get-any-no-auth', getAnyNoAuth)
router.post('/get-any-auth', auth, getAnyAuth)
router.post('/get-any-to-number-specified', auth, getAnyToNumberSpecified)

export default router
