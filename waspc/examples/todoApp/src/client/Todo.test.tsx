import { expect, test } from 'vitest'
import { areThereAnyTasks } from './Todo'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})
