import { QueryClient, type QueryClientConfig } from "@tanstack/react-query";
import { describe, expect, it } from "vitest";
import { getWaspQueryClientDefaults } from "../src/browser/operations/queryClient";
import { WaspHttpError } from "../src/WaspHttpError";

describe("query retries", () => {
  it.each([400, 401, 403, 404, 422])(
    "doesn't retry a query that fails with %i",
    async (statusCode) => {
      const attempts = await countQueryAttempts(
        new WaspHttpError(statusCode, "Client error", undefined),
      );
      expect(attempts).toBe(1);
    },
  );

  it.each([408, 429, 500, 503])(
    "retries a query that fails with %i",
    async (statusCode) => {
      const attempts = await countQueryAttempts(
        new WaspHttpError(statusCode, "Error", undefined),
      );
      expect(attempts).toBe(4);
    },
  );

  it("retries a query that fails with a non-HTTP error", async () => {
    const attempts = await countQueryAttempts(new Error("Network error"));
    expect(attempts).toBe(4);
  });

  it("lets the user override the retries", async () => {
    const waspDefaults = getWaspQueryClientDefaults();
    const attempts = await countQueryAttempts(
      new WaspHttpError(404, "Not found", undefined),
      {
        ...waspDefaults,
        defaultOptions: {
          ...waspDefaults.defaultOptions,
          queries: { ...waspDefaults.defaultOptions?.queries, retry: 2 },
        },
      },
    );
    expect(attempts).toBe(3);
  });
});

async function countQueryAttempts(
  error: unknown,
  config: QueryClientConfig = getWaspQueryClientDefaults(),
): Promise<number> {
  const queryClient = new QueryClient(config);

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
