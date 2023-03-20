// Relative imports/wasp stuff fine
// Q: wrapping vitest? see what it feels like when we wrap the Jest compatible stuff so client/server matches
// Keeping it explicit does set expectations a bit easier though
// How much effort to make it feel the same on server/client? But at what cost?
// Maybe punt on React testing for now, only unit test, add unit test support to backend
// Also want 'test watch' (need wasp watch under covers - what if start is running too?)
// How does this test phase interact with other parts of dev? maybe we just run `test watch` during start
// But does this require adding a terminal UI now, or can it wait?
// does `vitest watch` give you a nice UI already?
// TODO: check out a react project that uses vitest to see what they are testing and replicate to see if this setup breaks.
// Escape hatch: Pass anything beyond test to vitest.

import { screen } from '@testing-library/react'

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
