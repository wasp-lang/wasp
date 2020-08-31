import state from './state.js'

export const createTask = (task, context) => {
  if (Math.random() < 0.5) {
    throw new Error('Failed to create task, random error!')
  }
  state.tasks = [...(state.tasks || []), task]
}
