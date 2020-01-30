import * as reduxStarterKit from 'redux-starter-kit'

import loggerMiddleware from './middleware/logger'

/**
 * @param {Function} reducer - Redux reducer to be used for this store.
 * @param {Object} [preloadedState] - State that this store will be initialized with.
 * @returns {Object} Redux store, configured to suit our needs.
 */
export const configureStore = (reducer, preloadedState) => {
  const middleware = [
    loggerMiddleware,
    ...reduxStarterKit.getDefaultMiddleware()
  ]

  const enhancers = []

  const store = reduxStarterKit.configureStore({
    reducer,
    preloadedState,
    middleware,
    enhancers
  })

  return store
}
