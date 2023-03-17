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

import { BrowserRouter as Router } from 'react-router-dom'
import { expect, test, beforeAll, afterEach, afterAll } from 'vitest'
import { rest } from 'msw'
import { setupServer } from 'msw/node'
import { render, screen } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'

import getTasks from '@wasp/queries/getTasks.js'
import Todo, { areThereAnyTasks } from './Todo'

const server = setupServer()

beforeAll(() => server.listen())
afterEach(() => server.resetHandlers())
afterAll(() => server.close())

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

function renderWithClient(ui: React.ReactElement) {
  const client = new QueryClient()
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}><Router>{ui}</Router></QueryClientProvider>
  )
  return {
    ...result,
    rerender: (rerenderUi: React.ReactElement) =>
      rerender(
        <QueryClientProvider client={client}><Router>{rerenderUi}</Router></QueryClientProvider>
      ),
  }
}

function mockQuery(query, resJson) {
  const route = query.queryCacheKey[0]
  server.use(
    rest.post(`http://localhost:3001/${route}`, (req, res, ctx) => {
      return res(ctx.json(resJson))
    })
  )
}

const mockTasks = [{
  id: 1,
  description: 'test todo 1',
  isDone: false,
  userId: 1
}]

test('handles server error', async () => {
  mockQuery(getTasks, mockTasks);

  renderWithClient(<Todo />)

  await screen.findByText('test todo 1')

  screen.debug()
})
