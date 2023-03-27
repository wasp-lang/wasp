import React from 'react'
import { rest } from 'msw'
import { setupServer } from 'msw/node'
import { BrowserRouter as Router } from 'react-router-dom'
import { render, RenderResult, cleanup } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { beforeAll, afterEach, afterAll } from 'vitest'
import { Query } from '../../queries'
import config from '../../config'
import { type HttpMethod } from '../../types'

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

type QueryRoute = Query<any, any>['route']
type Handler = () => void

export function mockServer() {
  const server = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => {
    server.resetHandlers()
    cleanup()
  })
  afterAll(() => server.close())

  function mockQuery({ route }: { route: QueryRoute }, resJson: any): void {
    const url = `${config.apiUrl}${route.path}`
    const responseHandler = (_req, res, ctx) => {
      return res(ctx.json(resJson))
    }

    // NOTE: Technically, we only need to care about POST for Queries
    // and GET for the /auth/me route. However, an additional use case
    // for this function could be to mock APIs, so more methods are supported.
    const handlers: Record<HttpMethod, Handler> = {
      GET: () => {
        server.use(rest.get(url, responseHandler))
      },
      POST: () => {
        server.use(rest.post(url, responseHandler))
      },
      PUT: () => {
        server.use(rest.put(url, responseHandler))
      },
      DELETE: () => {
        server.use(rest.delete(url, responseHandler))
      },
    }

    const setupMock = handlers[route.method]
    if (!setupMock) {
      throw new Error(`Unsupported query method for mocking: ${route.method}. Supported method strings are: ${Object.keys(handlers).join(', ')}.`)
    }

    setupMock()
  }

  return { server, mockQuery }
}
