import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockQuery, renderWrapped } from '@wasp/vitest.helpers'
import getTasks from '@wasp/queries/getTasks'
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

  renderWrapped(<Todo />)

  await screen.findByText('test todo 1')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
