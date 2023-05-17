export function getTotalTaskCountMessage(numTasks?: number): string {
  if (numTasks === undefined) {
    return ''
  }
  if (numTasks === 0) {
    return 'No tasks created, yet.'
  }
  if (numTasks === 1) {
    return 'There is just one task.'
  }
  return `There are ${numTasks} tasks created so far.`
}
