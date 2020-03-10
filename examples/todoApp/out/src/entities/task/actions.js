import * as types from './actionTypes'
import Task from './Task'
import { selectors } from './state'


/**
 * @param {Task} task
 */
export const add = (task) => ({
  type: types.ADD,
  data: task.toData()
})

/**
 * @param {Task[]} tasks
 */
export const set = (tasks) => ({
    type: types.SET,
    tasks: tasks.map(t => t.toData())
})

/**
 * @param {String} id
 * @param {Object} data - Partial data that will be merged with existing task.
 */
export const update = (id, data) => ({
  type: types.UPDATE,
  id,
  data
})

/**
 * @param {String} id
 */
export const remove = (id) => ({
  type: types.REMOVE,
  id
})

export const toggleIsDoneAction = () => (dispatch, getState) => {
    const tasks = selectors.all(getState())
    const updateFn = tasks => {
    const areAllDone = tasks.every(t => t.isDone)
    return tasks.map(t => ({ ...t, isDone: !areAllDone }))
  }
    const newTasks = updateFn(tasks.map(t => t.toData())).map(t => new Task(t))
    dispatch(set(newTasks))
}

export const deleteDoneAction = () => (dispatch, getState) => {
    const tasks = selectors.all(getState())
    const updateFn = tasks => tasks.filter(t => !t.isDone)
    const newTasks = updateFn(tasks.map(t => t.toData())).map(t => new Task(t))
    dispatch(set(newTasks))
}

