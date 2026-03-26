import { afterEach, describe, expect, test, vi } from "vitest";
import { retryOnTransientError } from "../../src/common/retry.js";

vi.mock("../../src/common/terminal.js", () => ({
  waspSays: vi.fn(),
}));

import { waspSays } from "../../src/common/terminal.js";

const transientError = new Error("Failed to fetch: operation timed out");
const permanentError = new Error("Unauthorized");

const isRetryable = (error: unknown) =>
  error instanceof Error && error.message.includes("Failed to fetch");

afterEach(() => {
  vi.clearAllMocks();
});

describe("retryOnTransientError", () => {
  test("returns result on first attempt success", async () => {
    const fn = vi.fn().mockResolvedValue("ok");

    const result = await retryOnTransientError(fn, { isRetryable });

    expect(result).toBe("ok");
    expect(fn).toHaveBeenCalledTimes(1);
    expect(waspSays).not.toHaveBeenCalled();
  });

  test("retries on transient error and succeeds", async () => {
    const fn = vi
      .fn()
      .mockRejectedValueOnce(transientError)
      .mockResolvedValue("ok");

    const result = await retryOnTransientError(fn, { isRetryable });

    expect(result).toBe("ok");
    expect(fn).toHaveBeenCalledTimes(2);
    expect(waspSays).toHaveBeenCalledWith(
      "Command failed due to a network issue, retrying (attempt 2/3)...",
    );
  });

  test("throws last error when all attempts are exhausted", async () => {
    const fn = vi.fn().mockRejectedValue(transientError);

    await expect(
      retryOnTransientError(fn, { isRetryable }),
    ).rejects.toThrow(transientError);

    expect(fn).toHaveBeenCalledTimes(3);
    expect(waspSays).toHaveBeenCalledTimes(2);
  });

  test("throws immediately on non-retryable error", async () => {
    const fn = vi.fn().mockRejectedValue(permanentError);

    await expect(
      retryOnTransientError(fn, { isRetryable }),
    ).rejects.toThrow(permanentError);

    expect(fn).toHaveBeenCalledTimes(1);
    expect(waspSays).not.toHaveBeenCalled();
  });

  test("respects custom maxAttempts", async () => {
    const fn = vi.fn().mockRejectedValue(transientError);

    await expect(
      retryOnTransientError(fn, { maxAttempts: 5, isRetryable }),
    ).rejects.toThrow(transientError);

    expect(fn).toHaveBeenCalledTimes(5);
    expect(waspSays).toHaveBeenCalledTimes(4);
  });
});
