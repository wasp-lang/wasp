// @vitest-environment node
import { describe, it, expect } from 'vitest'
import { createTask } from './actions'

describe('createTask', () => {
  it('throws 401 when user is not authenticated', async () => {
    const unauthenticatedContext = { user: null, entities: {} as any }
    await expect(
      createTask({ description: 'Test task', tagIds: [] }, unauthenticatedContext as any)
    ).rejects.toMatchObject({ statusCode: 401 })
  })
})
