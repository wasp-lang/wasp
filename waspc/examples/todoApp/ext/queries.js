import state from './state.js'

export const getTasks = async (args, context) => {
  if (Math.random() < 0.5) {
    throw new Error('Random error: getting tasks failed.')
  }
  return state.tasks || []
}
