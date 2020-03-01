{{={= =}=}}
import { createSelector } from 'reselect'

import {=entityClassName=} from './{=entityClassName=}'
import * as types from './actionTypes'


// We assume that root reducer of the app will put this reducer under
// key ROOT_REDUCER_KEY.
const ROOT_REDUCER_KEY = 'entities/{=entity.name=}'

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

    case types.SET:
      return {
          ...state,
          all: action.{=_entities=}
      }

    case types.UPDATE:
      return {
        ...state,
        all: state.all.map(
          {=_entity=} =>
            {=_entity=}.id === action.id
              ? { ...{=_entity=}, ...action.data }
              : {=_entity=}
        )
      }

    case types.REMOVE:
      return {
        ...state,
        all: state.all.filter(
          {=_entity=} => {=_entity=}.id !== action.id
        )
      }

    default:
      return state
  }
}


let selectors = {}
selectors.root = (state) => state[ROOT_REDUCER_KEY]

/**
 * @returns {{= entity.name =}[]}
 */
selectors.all = createSelector(selectors.root, (state) => {
  return state.all.map(data => new {=entityClassName=}(data))
})


export { reducer, initialState, selectors, ROOT_REDUCER_KEY }
