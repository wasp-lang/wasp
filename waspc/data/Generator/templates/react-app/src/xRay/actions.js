import * as types from './actionTypes'

export const set = (isXRayModeOn) => ({
  type: types.SET,
  data: isXRayModeOn
})
