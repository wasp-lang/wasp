import * as RTK from '@reduxjs/toolkit'

import loggerMiddleware from './middleware/logger'

/**
 * @param {Function} reducer - Redux reducer to be used for this store.
 * @param {Object} [preloadedState] - State that this store will be initialized with.
 * @returns {Object} Redux store, configured to suit our needs.
 */
export const configureStore = (reducer, preloadedState) => {
  const middleware = [
    ...RTK.getDefaultMiddleware(),
    loggerMiddleware
  ]

  const enhancers = []

  const store = RTK.configureStore({
    reducer,
    preloadedState,
    middleware,
    enhancers
  })

  return store
}
