import type { ReactElement, ReactNode } from 'react'
import { render, type RenderResult } from '@testing-library/react'
import { http, type HttpResponseResolver, type RequestHandler } from 'msw'
import { setupServer, type SetupServer } from 'msw/node'
import { afterAll, afterEach, beforeAll } from 'vitest'
import { WaspTestWrapper } from 'wasp/client/test'
import { config, HttpMethod, type Route } from 'wasp/client'
import { serialize } from 'wasp/core/serialization'

export type MockQuery = (query: { route: Route }, resJson: unknown) => void

export type MockApi = (route: Route, resJson: unknown) => void

export function renderInContext(ui: ReactElement): RenderResult {
  const { rerender, ...result } = render(ui, { wrapper: WaspTestWrapper })
  return {
    ...result,
    rerender: (rerenderUi: ReactNode) => rerender(<WaspTestWrapper>{rerenderUi}</WaspTestWrapper>),
  }
}

export function mockServer(): {
  server: SetupServer
  mockQuery: MockQuery
  mockApi: MockApi
} {
  const server: SetupServer = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => server.resetHandlers())
  afterAll(() => server.close())

  const mockQuery: MockQuery = (query, mockData) => {
    mockRoute(server, query.route, () => Response.json(serialize(mockData)))
  }

  const mockApi: MockApi = (route, mockData) => {
    mockRoute(server, route, () => Response.json(mockData))
  }

  return { server, mockQuery, mockApi }
}

function mockRoute(
  server: SetupServer,
  route: Route,
  responseHandler: HttpResponseResolver
) {
  if (!Object.values(HttpMethod).includes(route.method)) {
    throw new Error(
      `Unsupported query method for mocking: ${route.method}. Supported method strings are: ${Object.values(HttpMethod).join(', ')}.`
    )
  }

  const url = `${config.apiUrl}${route.path}`

  const handlers: Record<HttpMethod, RequestHandler> = {
    [HttpMethod.Get]: http.get(url, responseHandler),
    [HttpMethod.Post]: http.post(url, responseHandler),
    [HttpMethod.Put]: http.put(url, responseHandler),
    [HttpMethod.Delete]: http.delete(url, responseHandler),
  }

  server.use(handlers[route.method])
}
