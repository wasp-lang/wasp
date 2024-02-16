var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
import { serialize as superjsonSerialize } from 'superjson';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { BrowserRouter as Router } from 'react-router-dom';
import { render, cleanup } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { beforeAll, afterEach, afterAll } from 'vitest';
import config from 'wasp/core/config';
import { HttpMethod } from 'wasp/client';
// PUBLIC API
// Inspired by the Tanstack React Query helper:
// https://github.com/TanStack/query/blob/4ae99561ca3383d6de3f4aad656a49ba4a17b57a/packages/react-query/src/__tests__/utils.tsx#L7-L26
export function renderInContext(ui) {
    const client = new QueryClient();
    const _a = render(<QueryClientProvider client={client}>
      <Router>{ui}</Router>
    </QueryClientProvider>), { rerender } = _a, result = __rest(_a, ["rerender"]);
    return Object.assign(Object.assign({}, result), { rerender: (rerenderUi) => rerender(<QueryClientProvider client={client}>
          <Router>{rerenderUi}</Router>
        </QueryClientProvider>) });
}
// PUBLIC API
export function mockServer() {
    const server = setupServer();
    beforeAll(() => server.listen());
    afterEach(() => {
        server.resetHandlers();
        cleanup();
    });
    afterAll(() => server.close());
    const mockQuery = (query, mockData) => {
        const route = query.route;
        mockRoute(server, route, (_req, res, ctx) => res(ctx.json(superjsonSerialize(mockData))));
    };
    const mockApi = (route, mockData) => {
        mockRoute(server, route, (_req, res, ctx) => res(ctx.json(mockData)));
    };
    return { server, mockQuery, mockApi };
}
function mockRoute(server, route, responseHandler) {
    if (!Object.values(HttpMethod).includes(route.method)) {
        throw new Error(`Unsupported query method for mocking: ${route.method}. Supported method strings are: ${Object.values(HttpMethod).join(', ')}.`);
    }
    const url = `${config.apiUrl}${route.path}`;
    const handlers = {
        [HttpMethod.Get]: rest.get(url, responseHandler),
        [HttpMethod.Post]: rest.post(url, responseHandler),
        [HttpMethod.Put]: rest.put(url, responseHandler),
        [HttpMethod.Delete]: rest.delete(url, responseHandler),
    };
    server.use(handlers[route.method]);
}
//# sourceMappingURL=helpers.jsx.map