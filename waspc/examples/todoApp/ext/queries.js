import HttpError from '@wasp/core/HttpError.js'

import state from './state.js'

export const getTasks = async (args, context) => {
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Random error: getting tasks failed.')
  }
  return state.tasks || []
}
