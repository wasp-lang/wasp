import { render, type RenderResult } from "@testing-library/react";
import { http } from "msw";
import { setupServer } from "msw/node";
import type { ReactElement, ReactNode } from "react";
import { afterAll, afterEach, beforeAll } from "vitest";
import {
  WaspTestWrapper,
  getOperationRoute,
  getApiUrl,
  serializeForResponse,
  type Route,
} from "wasp/client/test";

const httpMethodToHandler = {
  GET: http.get,
  POST: http.post,
  PUT: http.put,
  DELETE: http.delete,
} as const;

export function renderInContext(ui: ReactElement): RenderResult {
  const { rerender, ...result } = render(ui, { wrapper: WaspTestWrapper });
  return {
    ...result,
    rerender: (rerenderUi: ReactNode) =>
      rerender(<WaspTestWrapper>{rerenderUi}</WaspTestWrapper>),
  };
}

export function mockServer() {
  const server = setupServer();

  beforeAll(() => server.listen());
  afterEach(() => server.resetHandlers());
  afterAll(() => server.close());

  return {
    server,
    mockQuery(query: { route: Route }, mockData: unknown) {
      const { method, url } = getOperationRoute(query);
      server.use(
        httpMethodToHandler[method](url, () =>
          Response.json(serializeForResponse(mockData)),
        ),
      );
    },
    mockApi(route: Route, mockData: unknown) {
      server.use(
        httpMethodToHandler[route.method](getApiUrl(route), () =>
          Response.json(mockData),
        ),
      );
    },
  };
}
