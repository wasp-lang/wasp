import { waspSays } from "./terminal.js";

export interface RetryOptions {
  maxAttempts?: number;
  isRetryable: (error: unknown) => boolean;
}

const DEFAULT_MAX_ATTEMPTS = 3;

export async function retryOnTransientError<T>(
  fn: () => Promise<T>,
  options: RetryOptions,
): Promise<T> {
  const maxAttempts = options.maxAttempts ?? DEFAULT_MAX_ATTEMPTS;

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
          `Command failed due to a network issue, retrying (attempt ${attempt + 1}/${maxAttempts})...`,
        );
      }
    }
  }

  throw lastError;
}
