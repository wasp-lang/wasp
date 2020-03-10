import { createSelector } from 'reselect'

import * as types from './actionTypes'

// We assume that root reducer of the app will put this reducer under
// key ROOT_REDUCER_KEY.
const ROOT_REDUCER_KEY = 'xRay'

const initialState = {
  isXRayModeOn: false
}

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case types.SET:
      return {
        ...state,
        isXRayModeOn: action.data
      }

    default:
      return state
  }
}

let selectors = {}
selectors.root = (state) => state[ROOT_REDUCER_KEY]

selectors.isXRayModeOn = createSelector(selectors.root, (state) => {
  return state.isXRayModeOn
})

export { reducer, initialState, selectors, ROOT_REDUCER_KEY }
