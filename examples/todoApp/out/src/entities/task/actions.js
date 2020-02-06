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
