import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockServer, renderInContext } from 'wasp/client/test'
import { Todo, areThereAnyTasks } from './Todo'
import { MainPage } from './MainPage'
import type { AuthUser } from 'wasp/auth'
import { getMe } from 'wasp/client/auth'
import { getTasks } from 'wasp/client/operations'
import { Tasks } from 'wasp/client/crud'
import { makeAuthUserIfPossible } from 'wasp/auth/user'

const mockTasks = [
  {
    id: 1,
    description: 'test todo 1',
    isDone: true,
    userId: 1,
  },
]

const mockAllTasks = [
  {
    id: 2,
    description: 'test todo 2',
    isDone: true,
    userId: 1,
  },
]

test('handles unit testing', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

test('handles rendering in context', () => {
  renderInContext(<Todo {...mockTasks[0]} />)

  expect(screen.getByText('test todo 1')).toBeInTheDocument()
})

const { mockQuery } = mockServer()

// Abusing leaky private API, but it's fine for testing.
// To whoever has to deal with this when we hide the private API:
// Sorry, but it's probably me anyway :)
//
// On a more serious note, if this function
// is useful in testing, perhaps we should make it public
const mockUser = makeAuthUserIfPossible({
  id: 12,
  address: 'test address',
  identities: {
    username: {
      id: '123',
    },
  },
})

test('handles mock data', async () => {
  mockQuery(getTasks, mockTasks)
  mockQuery(getMe, mockUser)
  mockQuery(Tasks.getAll.query, mockAllTasks)

  renderInContext(<MainPage user={mockUser} />)

  await screen.findByText('test todo 1')
  await screen.findByText('test todo 2')

  expect(screen.getAllByRole('checkbox')[0]).toBeChecked()

  screen.debug()
})
