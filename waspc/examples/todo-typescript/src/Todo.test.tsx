import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockServer, renderInContext } from 'wasp/client/test'
import { Todo, areThereAnyTasks } from './Todo'
import { MainPage } from './MainPage'
import type { AuthUser } from 'wasp/auth'
import { getMe } from 'wasp/client/auth'
import { getTasks } from 'wasp/client/operations'
import { Tasks } from 'wasp/client/crud'

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

const mockUser = {
  identities: {
    username: { 
      id: 'Elon' 
    },
  },
} as AuthUser

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
