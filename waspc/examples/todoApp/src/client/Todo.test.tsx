// Not wrapping for now (had questions about making global)
// Since watch runs with test, should not run with start
// Passess any extra args to vitest

import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockQuery, renderWithClient } from '@wasp/../vitest.helpers.js'
import getTasks from '@wasp/queries/getTasks.js'
import Todo, { areThereAnyTasks } from './Todo'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

const mockTasks = [{
  id: 1,
  description: 'test todo 1',
  isDone: true,
  userId: 1
}]

test('handles mock data', async () => {
  mockQuery(getTasks, mockTasks);

  renderWithClient(<Todo />)

  await screen.findByText('test todo 1')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
