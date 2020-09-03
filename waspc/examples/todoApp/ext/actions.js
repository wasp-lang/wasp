import HttpError from '@wasp/core/HttpError.js'

import state from './state.js'

export const createTask = (task, context) => {
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Failed to create task, random error!')
  }
  state.tasks = [...(state.tasks || []), task]
}
