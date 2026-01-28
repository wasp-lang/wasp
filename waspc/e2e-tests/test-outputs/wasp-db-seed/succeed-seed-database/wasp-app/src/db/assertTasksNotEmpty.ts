import { prisma } from 'wasp/server'

export async function assertTasksNotEmpty() {
  const taskCount = await prisma.task.count()
  if (taskCount === 0) {
    throw new Error('Expected tasks table to have data, but it was empty')
  }
}