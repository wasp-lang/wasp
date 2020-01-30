import * as types from './actionTypes'


/**
 * @param {Task} task
 */
export const add = (task) => ({
  type: types.ADD,
  data: task.toData()
})

/**
 * @param {String} id
 * @param {Task} updatedTask
 */
export const update = (id, updatedTask) => ({
  type: types.UPDATE,
  id,
  data: updatedTask.toData()
})

/**
 * @param {String} id
 */
export const remove = (id) => ({
  type: types.REMOVE,
  id
})
