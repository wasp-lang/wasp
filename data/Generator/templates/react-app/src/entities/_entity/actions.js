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
 * @param {Number} idx
 * @param {{= entity.name =}} updated{= entity.name =}
 */
export const update = (idx, updated{= entity.name =}) => ({
  type: types.UPDATE,
  idx,
  data: updated{= entity.name =}.toData()
})
