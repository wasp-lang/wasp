import { ProcessOutput } from "zx";
import { retryOnTransientError } from "../../common/retry.js";

export function retryOnRailwayAPIError<T>(fn: () => Promise<T>): Promise<T> {
  return retryOnTransientError(fn, {
    isRetryable: isRailwayTransientError,
    retryDescription: "a Railway API issue",
  });
}

export function isRailwayTransientError(error: unknown): boolean {
  if (!(error instanceof ProcessOutput)) {
    return false;
  }

  const stderr = error.stderr.toLowerCase();

  // Railway CLI prefixes network-level fetch failures with "failed to fetch:".
  if (stderr.includes("failed to fetch:")) {
    return TRANSIENT_FETCH_ERROR_PATTERNS.some((pattern) =>
      stderr.includes(pattern),
    );
  }

  if (stderr.includes("ratelimited")) {
    return true;
  }

  return false;
}

const TRANSIENT_FETCH_ERROR_PATTERNS = [
  "timed out",
  "connection refused",
  "dns error",
];
