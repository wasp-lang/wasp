import type { MockQuery, MockApi } from '@wasp.sh/lib-sdk-core/browser/test'
import { http, type HttpResponseResolver, type RequestHandler } from 'msw'
import { setupServer, type SetupServer } from 'msw/node'
import { cleanup } from '@testing-library/react'
import { beforeAll, afterEach, afterAll } from 'vitest'
import { config, HttpMethod, type Route } from '../../index.js'
import { serialize } from '../../../core/serialization/index.js'

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
    mockRoute(server, route, () => Response.json(serialize(mockData)))
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
      `Unsupported query method for mocking: ${
        route.method
      }. Supported method strings are: ${Object.values(HttpMethod).join(', ')}.`
    )
  }

  const url = `${config.apiUrl}${route.path}`

  const handlers: Record<HttpMethod, RequestHandler> = {
    [HttpMethod.Get]: http.get(url, responseHandler),
    [HttpMethod.Post]: http.post(url, responseHandler),
    [HttpMethod.Put]: http.put(url, responseHandler),
    [HttpMethod.Patch]: http.patch(url, responseHandler),
    [HttpMethod.Delete]: http.delete(url, responseHandler),
    [HttpMethod.Head]: http.head(url, responseHandler),
  }

  server.use(handlers[route.method])
}
