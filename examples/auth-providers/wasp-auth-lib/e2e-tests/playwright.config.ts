import { defineConfig } from "@playwright/test";

const WASP_APP_RUNNER_CLI_CMD =
  process.env.WASP_APP_RUNNER_CLI_CMD ?? "run-wasp-app";
const WASP_RUN_MODE = process.env.WASP_RUN_MODE ?? "dev";
const WASP_CLI_CMD = process.env.WASP_CLI_CMD ?? "wasp-cli";

export const WASP_SERVER_PORT = 3001;
export const WASP_SERVER_URL = `http://localhost:${WASP_SERVER_PORT}`;

/**
 * Runtime coverage for the credential exchange and provider-attributed
 * sessions -- API-level specs (no browser pages), so a single project runs
 * them.
 */
export default defineConfig({
  testDir: "./tests",
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  reporter: process.env.CI ? "dot" : "list",
  use: {
    baseURL: WASP_SERVER_URL,
  },
  projects: [{ name: "api" }],
  webServer: {
    command: `${WASP_APP_RUNNER_CLI_CMD} ${WASP_RUN_MODE} --path-to-app=../ --wasp-cli-cmd=${WASP_CLI_CMD}`,
    url: WASP_SERVER_URL,
    reuseExistingServer: !process.env.CI,
    timeout: 180 * 1000,
    gracefulShutdown: { signal: "SIGTERM", timeout: 500 },
  },
});
