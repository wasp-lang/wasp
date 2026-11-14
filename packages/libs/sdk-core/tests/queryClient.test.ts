import type { QueryClientConfig } from "@tanstack/react-query";
import { beforeEach, describe, expect, it, vi } from "vitest";

describe("query retries", () => {
  let HttpError: typeof import("../src/HttpError").HttpError;

  beforeEach(async () => {
    // The query client lives in module state, so each test needs fresh
    // modules. `HttpError` must come from the same module instance for
    // `instanceof` checks to work.
    vi.resetModules();
    ({ HttpError } = await import("../src/HttpError"));
  });

  it.each([400, 401, 403, 404, 422])(
    "doesn't retry a query that fails with %i",
    async (statusCode) => {
      const attempts = await countQueryAttempts(
        new HttpError(statusCode, "Client error"),
      );
      expect(attempts).toBe(1);
    },
  );

  it.each([408, 429, 500, 503])(
    "retries a query that fails with %i",
    async (statusCode) => {
      const attempts = await countQueryAttempts(
        new HttpError(statusCode, "Error"),
      );
      expect(attempts).toBe(4);
    },
  );

  it("retries a query that fails with a non-HTTP error", async () => {
    const attempts = await countQueryAttempts(new Error("Network error"));
    expect(attempts).toBe(4);
  });

  it("keeps the default retries when the user configures other query options", async () => {
    const attempts = await countQueryAttempts(new HttpError(404, "Not found"), {
      defaultOptions: { queries: { staleTime: Infinity } },
    });
    expect(attempts).toBe(1);
  });

  it("lets the user override the retries", async () => {
    const attempts = await countQueryAttempts(new HttpError(404, "Not found"), {
      defaultOptions: { queries: { retry: 2 } },
    });
    expect(attempts).toBe(3);
  });
});

async function countQueryAttempts(
  error: unknown,
  config?: QueryClientConfig,
): Promise<number> {
  const {
    configureQueryClient,
    initializeQueryClient,
    queryClientInitialized,
  } = await import("../src/browser/operations/queryClient");

  if (config) {
    configureQueryClient(config);
  }
  initializeQueryClient();
  const queryClient = await queryClientInitialized;

  let attempts = 0;
  await queryClient
    .fetchQuery({
      queryKey: ["query"],
      queryFn: () => {
        attempts++;
        throw error;
      },
      retryDelay: 0,
    })
    .catch(() => {});

  return attempts;
}
