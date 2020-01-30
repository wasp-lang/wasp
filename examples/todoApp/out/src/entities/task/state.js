import { createSelector } from 'reselect'

import Task from './Task'
import * as types from './actionTypes'


// We assume that root reducer of the app will put this reducer under
// key ROOT_REDUCER_KEY.
const ROOT_REDUCER_KEY = 'entities/Task'

const initialState = {
  all: []
}

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case types.ADD:
      return {
        ...state,
        all: [ ...state.all, action.data ]
      }

    case types.UPDATE:
      return {
        ...state,
        all: state.all.map(
          task => task.id === action.id ? action.data : task
        )
      }

    case types.REMOVE:
      return {
        ...state,
        all: state.all.filter(
          task => task.id !== action.id
        )
      }

    default:
      return state
  }
}


let selectors = {}
selectors.root = (state) => state[ROOT_REDUCER_KEY]

/**
 * @returns {Task[]}
 */
selectors.all = createSelector(selectors.root, (state) => {
  return state.all.map(data => new Task(data))
})


export { reducer, initialState, selectors, ROOT_REDUCER_KEY }
