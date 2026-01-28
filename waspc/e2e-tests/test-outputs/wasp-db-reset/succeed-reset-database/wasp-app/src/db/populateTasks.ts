import { prisma } from 'wasp/server'

export async function populateTasks() {
  await prisma.task.create({
    data: { description: 'Test task', isDone: false }
  })
}