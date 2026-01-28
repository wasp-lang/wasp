import { prisma } from 'wasp/server'

export async function assertTasksEmpty() {
  const taskCount = await prisma.task.count()
  if (taskCount !== 0) {
    throw new Error(`Expected tasks table to be empty, but found ${taskCount} tasks`)
  }
}