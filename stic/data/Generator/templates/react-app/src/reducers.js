{{={= =}=}}
import { combineReducers } from 'redux'

// import * as dataState from './modules/data/state'


const states = [
  // dataState,
  // Add reducer here to add it to the app.
]

const keyToReducer = states.reduce((acc, state) => {
  // We set the reducers here by using their ROOT_REDUCER_KEY, because their
  // internal state implementations assume so.
  return { ...acc, [state.ROOT_REDUCER_KEY]: state.reducer }
}, {})

export const rootReducer = combineReducers({
  ...keyToReducer
})
