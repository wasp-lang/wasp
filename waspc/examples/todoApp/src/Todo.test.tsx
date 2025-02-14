import { type AuthUser } from 'wasp/auth'
import { mockServer, renderInContext } from 'wasp/client/test'
import { getTasks, getDate } from 'wasp/client/operations'
import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import Todo, { areThereAnyTasks } from './Todo'
import { App } from './App'
import { getMe } from 'wasp/client/auth'

import { Routes, Route } from 'react-router-dom'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

const { mockQuery } = mockServer()

const mockTasks = [
  {
    id: 1,
    description: 'test todo 1',
    isDone: true,
    userId: 1,
    user: {
      id: 1,
      isOnAfterSignupHookCalled: true,
      isOnAfterLoginHookCalled: true,
      address: '123 Main St',
      auth: {
        id: '1',
        userId: 1,
        identities: [
          {
            providerName: 'email',
            providerUserId: 'elon@tesla.com',
          },
        ],
      },
    },
  },
]

test('handles mock data', async () => {
  mockQuery(getTasks, mockTasks)

  renderInContext(<Todo />)

  await screen.findByText('test todo 1')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})

const mockUser = {
  identities: {
    email: {
      id: 'elon@tesla.com',
    },
  },
} as AuthUser

test('handles multiple mock data sources', async () => {
  mockQuery(getMe, mockUser)
  mockQuery(getDate, new Date())
  mockQuery(getTasks, mockTasks)

  renderInContext(
    <Routes>
      <Route path="/" Component={App}>
        <Route path="/" Component={Todo} />
      </Route>
    </Routes>
  )

  await screen.findByText('elon@tesla.com')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
