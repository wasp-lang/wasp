import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockServer, renderInContext } from '@wasp/test'
import getTasks from '@wasp/queries/getTasks'
import Todo, { areThereAnyTasks } from './Todo'
import { App } from './App'
import { getMe } from '@wasp/auth/useAuth'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

const { mockQuery } = mockServer()

const mockTasks = [{
  id: 1,
  description: 'test todo 1',
  isDone: true,
  userId: 1
}]

test('handles mock data', async () => {
  mockQuery(getTasks, mockTasks)

  renderInContext(<Todo />)

  await screen.findByText('test todo 1')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})

test('handles multiple mock data sources', async () => {
  mockQuery(getMe, { id: 12, username: 'elon' })
  mockQuery(getTasks, mockTasks)

  renderInContext(<App><Todo /></App>)

  await screen.findByText('elon')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
