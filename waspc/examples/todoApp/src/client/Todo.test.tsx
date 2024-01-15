import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockServer, renderInContext } from '@wasp/test'
import getTasks from '@wasp/queries/getTasks'
import getDate from '@wasp/queries/getDate'
import Todo, { areThereAnyTasks } from './Todo'
import { App } from './App'
import { getMe } from '@wasp/auth/useAuth'
import type { User } from '@wasp/auth/types'

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
  id: 12,
  auth: {
    id: '123',
    userId: 12,
    identities: [
      {
        authId: '123',
        providerName: 'email',
        providerUserId: 'elon@tesla.com',
        providerData: '',
      },
    ],
  },
  address: null,
} satisfies User

test('handles multiple mock data sources', async () => {
  mockQuery(getMe, mockUser)
  mockQuery(getDate, new Date())
  mockQuery(getTasks, mockTasks)

  renderInContext(
    <App>
      <Todo />
    </App>
  )

  await screen.findByText('elon@tesla.com')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
