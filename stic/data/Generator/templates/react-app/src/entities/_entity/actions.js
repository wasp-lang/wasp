{{={= =}=}}
import * as types from './actionTypes'


/**
 * @param {{= entity.name =}} {= entityLowerName =}
 */
export const add = ({= entityLowerName =}) => ({
  type: types.ADD,
  data: {= entityLowerName =}.toData()
})
