import { ReactElement, ReactNode } from 'react'
import { rest, type ResponseResolver, type RestContext } from 'msw'
import { setupServer, type SetupServer } from 'msw/node'
import { BrowserRouter as Router } from 'react-router-dom'
import { render, RenderResult, cleanup } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { beforeAll, afterEach, afterAll } from 'vitest'
import { Query } from 'wasp/client/operations/rpc'
import { config } from 'wasp/client'
import { HttpMethod, Route } from 'wasp/client'
import { serialize } from 'wasp/core/serialization'

// PRIVATE API
export type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  resJson: MockOutput
) => void

// PRIVATE API
export type MockApi = (route: Route, resJson: unknown) => void

// PUBLIC API
// Inspired by the Tanstack React Query helper:
// https://github.com/TanStack/query/blob/4ae99561ca3383d6de3f4aad656a49ba4a17b57a/packages/react-query/src/__tests__/utils.tsx#L7-L26
export function renderInContext(ui: ReactElement): RenderResult {
  const client = new QueryClient()
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}>
      <Router>{ui}</Router>
    </QueryClientProvider>
  )
  return {
    ...result,
    rerender: (rerenderUi: ReactNode) =>
      rerender(
        <QueryClientProvider client={client}>
          <Router>{rerenderUi}</Router>
        </QueryClientProvider>
      ),
  }
}

// PUBLIC API
export function mockServer(): {
  server: SetupServer
  mockQuery: MockQuery
  mockApi: MockApi
} {
  const server: SetupServer = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => {
    server.resetHandlers()
    cleanup()
  })
  afterAll(() => server.close())

  const mockQuery: MockQuery = (query, mockData) => {
    const route = (query as unknown as { route: Route }).route
    mockRoute(server, route, (_req, res, ctx) =>
      res(ctx.json(serialize(mockData)))
    )
  }

  const mockApi: MockApi = (route, mockData) => {
    mockRoute(server, route, (_req, res, ctx) => res(ctx.json(mockData)))
  }

  return { server, mockQuery, mockApi }
}

function mockRoute(
  server: SetupServer,
  route: Route,
  responseHandler: ResponseResolver<any, RestContext, any>
) {
  if (!Object.values(HttpMethod).includes(route.method)) {
    throw new Error(
      `Unsupported query method for mocking: ${
        route.method
      }. Supported method strings are: ${Object.values(HttpMethod).join(', ')}.`
    )
  }

  const url = `${config.apiUrl}${route.path}`

  const handlers: Record<HttpMethod, Parameters<typeof server.use>[0]> = {
    [HttpMethod.Get]: rest.get(url, responseHandler),
    [HttpMethod.Post]: rest.post(url, responseHandler),
    [HttpMethod.Put]: rest.put(url, responseHandler),
    [HttpMethod.Delete]: rest.delete(url, responseHandler),
  }

  server.use(handlers[route.method])
}
