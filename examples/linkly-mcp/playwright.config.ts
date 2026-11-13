import { defineConfig, devices } from "@playwright/test";
import { CLIENT_URL, TEST_DATABASE_URL } from "./e2e/env";

// Live-server tests: end-to-end browser flows, the MCP integration test, CLI
// tests, and OpenAPI checks all run against a real `wasp start` using the
// separate `linkly_test` database. `wasp start` sets NODE_ENV=development.
export default defineConfig({
  testDir: ".",
  testMatch: ["e2e/**/*.spec.ts", "cli/test/**/*.spec.ts"],
  testIgnore: ["**/node_modules/**"],
  fullyParallel: false,
  workers: 1,
  timeout: 60_000,
  expect: { timeout: 10_000 },
  reporter: [["list"]],
  globalSetup: "./e2e/globalSetup.ts",
  use: {
    baseURL: CLIENT_URL,
    trace: "retain-on-failure",
    ...devices["Desktop Chrome"],
    headless: true,
  },
  webServer: {
    // Ports are pinned because e2e/env.ts hardcodes them; otherwise `wasp start` moves to free ones when these are taken.
    // WASP_CLI picks the Wasp CLI, e.g. the dev one from this checkout:
    // WASP_CLI=../../waspc/tools/wasp-cli-dev npm run test:e2e
    command: `npx prisma migrate deploy --schema schema.prisma && ${process.env.WASP_CLI ?? "wasp"} start --client-port 3000 --server-port 3001 < /dev/null`,
    url: CLIENT_URL,
    timeout: 180_000,
    reuseExistingServer: false,
    stdout: "ignore",
    stderr: "pipe",
    env: {
      DATABASE_URL: TEST_DATABASE_URL,
      LINKLY_ENABLE_TEST_API: "true",
      MCP_SECRET: "test-only-mcp-secret-0123456789abcdef",
    },
  },
});
