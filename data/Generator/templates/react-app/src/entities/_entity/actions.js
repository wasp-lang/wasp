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
 * @param {{= entity.name =}} updated{= entity.name =}
 */
export const update = (id, updated{= entity.name =}) => ({
  type: types.UPDATE,
  id,
  data: updated{= entity.name =}.toData()
})

/**
 * @param {String} id
 */
export const remove = (id) => ({
  type: types.REMOVE,
  id
})
