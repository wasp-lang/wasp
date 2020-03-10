{{={= =}=}}
import { combineReducers } from 'redux'

{=# entities =}
import * as {= entityLowerName =}State from '{= entityStatePath =}'
{=/ entities =}
{=# wasp.isXRayModeEnabled =}
import * as xRayState from './xRay/state.js'
{=/ wasp.isXRayModeEnabled =}


const states = [
  {=# entities =}
  {= entityLowerName =}State,
  {=/ entities =}
  {=# wasp.isXRayModeEnabled =}
  xRayState
  {=/ wasp.isXRayModeEnabled =}
]

const keyToReducer = states.reduce((acc, state) => {
  // We set the reducers here by using their ROOT_REDUCER_KEY, because their
  // internal state implementations assume so.
  return { ...acc, [state.ROOT_REDUCER_KEY]: state.reducer }
}, {})

export const rootReducer = combineReducers({
  ...keyToReducer
})
