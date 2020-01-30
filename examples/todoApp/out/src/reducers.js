import { combineReducers } from 'redux'

import * as taskState from './entities/task/state.js'


const states = [
  taskState,
]

const keyToReducer = states.reduce((acc, state) => {
  // We set the reducers here by using their ROOT_REDUCER_KEY, because their
  // internal state implementations assume so.
  return { ...acc, [state.ROOT_REDUCER_KEY]: state.reducer }
}, {})

export const rootReducer = combineReducers({
  ...keyToReducer
})
