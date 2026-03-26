import { ProcessOutput } from "zx";

const TRANSIENT_FETCH_PATTERNS = [
  "timed out",
  "connection refused",
  "dns error",
];

export function isRailwayTransientError(error: unknown): boolean {
  if (!(error instanceof ProcessOutput)) {
    return false;
  }

  const stderr = error.stderr.toLowerCase();

  if (stderr.includes("failed to fetch:")) {
    return TRANSIENT_FETCH_PATTERNS.some((pattern) =>
      stderr.includes(pattern),
    );
  }

  if (stderr.includes("ratelimited")) {
    return true;
  }

  if (stderr.includes("failed to upload code with status code 5")) {
    return true;
  }

  return false;
}
