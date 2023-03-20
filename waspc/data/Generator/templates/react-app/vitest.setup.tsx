import { beforeAll, afterEach, afterAll } from 'vitest'
import React from 'react'
import { rest } from 'msw'
import { setupServer } from 'msw/node'
import { BrowserRouter as Router } from 'react-router-dom'
import { render } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'

import '@testing-library/jest-dom/extend-expect'

const server = setupServer()

beforeAll(() => server.listen())
afterEach(() => server.resetHandlers())
afterAll(() => server.close())

// TODO: Add to tsconfig.json under types

global.renderWithClient = (ui: React.ReactElement) => {
  const client = new QueryClient()
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}><Router>{ui}</Router></QueryClientProvider>
  )
  return {
    ...result,
    rerender: (rerenderUi: React.ReactElement) =>
      rerender(
        <QueryClientProvider client={client}><Router>{rerenderUi}</Router></QueryClientProvider>
      )
  }
}

global.mockQuery = (query, resJson) => {
  const route = query.queryCacheKey[0]
  server.use(
    rest.post(`http://localhost:3001/${route}`, (req, res, ctx) => {
      return res(ctx.json(resJson))
    })
  )
}
