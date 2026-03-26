import assert from "node:assert";

import { waspSays } from "./terminal.js";

export interface RetryOptions {
  maxAttempts?: number;
  isRetryable: (error: unknown) => boolean;
  retryDescription?: string;
}

const DEFAULT_MAX_ATTEMPTS = 3;

export async function retryOnTransientError<T>(
  fn: () => Promise<T>,
  options: RetryOptions,
): Promise<T> {
  const maxAttempts = options.maxAttempts ?? DEFAULT_MAX_ATTEMPTS;

  assert(maxAttempts >= 1, "maxAttempts must be at least 1");

  const reason = options.retryDescription ?? "a transient error";

  let lastError: unknown;
  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;

      if (!options.isRetryable(error)) {
        throw error;
      }

      if (attempt < maxAttempts) {
        waspSays(
          `Command failed due to ${reason}, retrying (attempt ${attempt + 1}/${maxAttempts})...`,
        );
      }
    }
  }

  throw lastError;
}
