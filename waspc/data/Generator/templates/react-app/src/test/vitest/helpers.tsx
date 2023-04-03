import { ReactElement } from 'react'
import { rest, type ResponseResolver, type RestContext } from 'msw'
import { setupServer, type SetupServer } from 'msw/node'
import { BrowserRouter as Router } from 'react-router-dom'
import { render, RenderResult, cleanup } from '@testing-library/react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { beforeAll, afterEach, afterAll } from 'vitest'
import { Query } from '../../queries'
import config from '../../config'
import { HttpMethod } from '../../types'

// Inspired by the Tanstack React Query helper:
// https://github.com/TanStack/query/blob/4ae99561ca3383d6de3f4aad656a49ba4a17b57a/packages/react-query/src/__tests__/utils.tsx#L7-L26
export function renderInContext(ui: ReactElement): RenderResult {
  const client = new QueryClient()
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}><Router>{ui}</Router></QueryClientProvider>
  )
  return {
    ...result,
    rerender: (rerenderUi: ReactElement) =>
      rerender(
        <QueryClientProvider client={client}><Router>{rerenderUi}</Router></QueryClientProvider>
      )
  }
}

type QueryRoute = Query<any, any>['route']
type MockQuery = <Input, Output, MockData extends Output>(query: Query<Input, Output>, resJson: MockData) => void

export function mockServer(): {
  server: SetupServer,
  mockQuery: MockQuery,
} {
  const server: SetupServer = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => {
    server.resetHandlers()
    cleanup()
  })
  afterAll(() => server.close())

  const mockQuery : MockQuery = (query, mockData) => {
    const route = (query as InternalQuery<any, any>).route
    if (!Object.values(HttpMethod).includes(route.method)) {
      throw new Error(`Unsupported query method for mocking: ${route.method}. Supported method strings are: ${Object.values(HttpMethod).join(', ')}.`)
    }

    const url = `${config.apiUrl}${route.path}`
    const responseHandler: ResponseResolver<any, RestContext, any> = (_req, res, ctx) => {
      return res(ctx.json(mockData))
    }

    // NOTE: Technically, we only need to care about POST for Queries
    // and GET for the /auth/me route. However, an additional use case
    // for this function could be to mock APIs, so more methods are supported.
    const handlers: Record<HttpMethod, Parameters<typeof server.use>[0]> = {
      [HttpMethod.Get]: rest.get(url, responseHandler),
      [HttpMethod.Post]: rest.post(url, responseHandler),
      [HttpMethod.Put]: rest.put(url, responseHandler),
      [HttpMethod.Delete]: rest.delete(url, responseHandler),
    }

    server.use(handlers[route.method])
  }

  return { server, mockQuery }
}

type InternalQuery<Input, Output> = {
  (args: Input): Promise<Output>
  queryCacheKey: string[]
  route: { method: HttpMethod, path: string }
}
