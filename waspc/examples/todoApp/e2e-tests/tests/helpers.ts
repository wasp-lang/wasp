import { randomUUID } from "crypto";

export function isRunningInDevMode() {
  const testMode = process.env.WASP_RUN_MODE ?? "dev";
  return testMode === "dev";
}

export function generateRandomEmail(): string {
  return `${randomUUID()}@test.com`;
}
