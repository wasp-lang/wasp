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
import Todo, { areThereAnyTasks } from './Todo'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'

const server = setupServer()

beforeAll(() => server.listen())
afterEach(() => server.resetHandlers())
afterAll(() => server.close())

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})

function renderWithClient(client: QueryClient, ui: React.ReactElement) {
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}>{ui}</QueryClientProvider>
  )
  return {
    ...result,
    rerender: (rerenderUi: React.ReactElement) =>
      rerender(
        <QueryClientProvider client={client}>{rerenderUi}</QueryClientProvider>
      ),
  }
}

const mockTasks = [{
  id: 1,
  description: 'test todo 1',
  isDone: false,
  userId: 1
}]

function serverReturns(route, resJson) {
  server.use(
    rest.post(`http://localhost:3001/${route}`, (req, res, ctx) => {
      return res(ctx.json(resJson))
    })
  )
}

test('handles server error', async () => {
  serverReturns('operations/get-tasks', mockTasks);

  renderWithClient(new QueryClient(), <Router><Todo /></Router>)

  await screen.findByText('test todo 1')

  screen.debug()
})
