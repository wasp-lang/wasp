import { randomUUID } from "crypto";

export function isRunningInDevMode() {
  const testMode = process.env.WASP_RUN_MODE ?? "dev";
  return testMode === "dev";
}

export function isRunningInDeployedMode() {
  return process.env.WASP_RUN_MODE === "deployed";
}

export function generateRandomEmail(): string {
  return `${randomUUID()}@test.com`;
}

/**
 * Generates an address with non-ASCII characters on both sides of the `@`.
 */
export function generateRandomInternationalizedEmail(): string {
  return `jürgen-${randomUUID()}@münchen.test`;
}
