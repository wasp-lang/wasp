import { QueryObserver } from "@tanstack/react-query";
import { http, HttpResponse } from "msw";
import { beforeAll, describe, expect, it, vi } from "vitest";
import { config } from "wasp/client";
import {
  createTask,
  getTasks,
  initializeQueryClient,
  queryClientInitialized,
} from "wasp/client/operations";
import { mockServer } from "wasp/client/test";
import { serialize } from "wasp/core/serialization";

// Regression test for https://github.com/wasp-lang/wasp/issues/3009
//
// When an action declares `entities: [X]` and a query also depends on `X`, the
// query is supposed to be up to date by the time the action's promise resolves.
// `useAuth()` is just a special case of this: its `auth/me` query depends on the
// `User` entity, so an action that updates `User` (e.g. "complete profile")
// should leave the cached auth user fresh once it resolves.
//
// The bug: `registerActionDone` fires `queryClient.invalidateQueries(...)` for
// the affected queries but does NOT await the triggered refetches
// (see client/operations/internal/resources.js, `invalidateQueriesUsing`). So
// `await someAction()` returns while the refetch is still in flight, and code
// that runs right after (a redirect, a `user.x` check) sees stale data.
//
// We assert SYNCHRONOUSLY right after `await createTask(...)` that the
// `getTasks` cache already reflects the update. A `waitFor(...)` would pass even
// with the bug, because the refetch does eventually land. The whole point of the
// bug is the timing, so the assertion must not wait.

const { server } = mockServer();

// `getTasks` and `createTask` both declare `entities: ["Task"]`, so completing
// the action must invalidate (and, per the contract, refetch) `getTasks`.
type FakeTask = { id: number; description: string; isDone: boolean };

let tasksOnServer: FakeTask[] = [];

beforeAll(async () => {
  // `mockServer()` (called above) already wires up server.listen/resetHandlers/
  // close via beforeAll/afterEach/afterAll, so we only need to add our handler.
  initializeQueryClient();
  await queryClientInitialized;

  // One handler for every operation POST. `getTasks` returns the current server
  // state; any other operation (i.e. `createTask`) just acknowledges.
  const apiUrlPattern = new RegExp(
    "^" + config.apiUrl.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"),
  );
  server.use(
    http.post(apiUrlPattern, ({ request }) => {
      const { pathname } = new URL(request.url);
      if (pathname === getTasks.route.path) {
        return HttpResponse.json(serialize(tasksOnServer));
      }
      return HttpResponse.json(
        serialize({ id: 0, description: "", isDone: false }),
      );
    }),
  );
});

describe("action cache invalidation (#3009)", () => {
  it("queries depending on the action's entities are fresh once the action resolves", async () => {
    const queryClient = await queryClientInitialized;
    const queryKey = getTasks.queryCacheKey;

    // Server starts with a single task.
    tasksOnServer = [{ id: 1, description: "before", isDone: false }];

    // Mount the query (make it an active observer on the SDK's query client, the
    // same client the action invalidates) and wait for the initial fetch.
    const observer = new QueryObserver(queryClient, {
      queryKey,
      queryFn: () => getTasks(),
      retry: false,
    });
    const unsubscribe = observer.subscribe(() => {});

    try {
      await vi.waitFor(() =>
        expect(queryClient.getQueryData<FakeTask[]>(queryKey)).toHaveLength(1),
      );

      // The server now has a new task. Nothing has refetched it yet.
      tasksOnServer = [
        { id: 1, description: "before", isDone: false },
        { id: 2, description: "after", isDone: false },
      ];

      // Run an action that touches the `Task` entity.
      await createTask({ description: "after" });

      // The action has resolved, so the `getTasks` cache must already reflect the
      // update. With the bug, the invalidation refetch is still in flight here and
      // the cache still holds the single original task.
      expect(queryClient.getQueryData<FakeTask[]>(queryKey)).toHaveLength(2);
    } finally {
      unsubscribe();
    }
  });
});
