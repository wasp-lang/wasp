export function isRunningInDevMode() {
  const testMode = process.env.WASP_RUN_MODE ?? "dev";
  return testMode === "dev";
}
