import React from 'react'
import { rest } from 'msw'
import { setupServer } from 'msw/node'
import { BrowserRouter as Router } from 'react-router-dom'
import { render, RenderResult, cleanup } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { beforeAll, afterEach, afterAll } from 'vitest'
import { Query } from '../../queries'
import config from '../../config'

export function renderInContext(ui: React.ReactElement): RenderResult {
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

export function mockServer() {
  const server = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => {
    server.resetHandlers()
    cleanup()
  })
  afterAll(() => server.close())

  function mockQuery(query: Query<any, any>, resJson: any): void {
    const url = `${config.apiUrl}${query.route.path}`
    const responseHandler = (_req, res, ctx) => {
      return res(ctx.json(resJson))
    }

    switch (query.route.method) {
      case 'GET':
        server.use(rest.get(url, responseHandler))
        break
      case 'POST':
        server.use(rest.post(url, responseHandler))
        break
      default: throw new Error(`Unsupported method ${query.route.method}`)
    }
  }

  return { server, mockQuery }
}
