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
 * An address with non-ASCII characters on both sides of the `@`, the kind
 * RFC 6531 allows and the HTML5 `input[type=email]` grammar does not.
 */
export function generateRandomInternationalizedEmail(): string {
  return `jürgen-${randomUUID()}@münchen.test`;
}
