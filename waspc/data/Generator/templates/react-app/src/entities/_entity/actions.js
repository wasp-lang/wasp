{{={= =}=}}
import * as types from './actionTypes'


/**
 * @param {{= entity.name =}} {= entityLowerName =}
 */
export const add = ({= entityLowerName =}) => ({
  type: types.ADD,
  data: {= entityLowerName =}.toData()
})

/**
 * @param {String} id
 * @param {Object} data - Partial data that will be merged with existing {= entityLowerName =}.
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
