import { defineConfig } from "vitest/config";

// Server-side unit tests, run with `npx vitest run` (outside `wasp test`,
// which only supports client tests). These tests import framework-free code
// only: never `wasp/*`.
export const TEST_DATABASE_URL =
  process.env.TEST_DATABASE_URL ??
  "postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/linkly_mcp_test";

export default defineConfig({
  test: {
    include: ["src/**/*.test.ts", "tests/**/*.test.ts"],
    environment: "node",
    globalSetup: ["./src/test/globalSetup.ts"],
    fileParallelism: false, // tests share one database
    env: {
      NODE_ENV: "test",
      DATABASE_URL: TEST_DATABASE_URL,
      WASP_SERVER_URL: "http://localhost:3001",
      WASP_WEB_CLIENT_URL: "http://localhost:3000",
      MCP_SECRET: "test-only-mcp-secret-0123456789abcdef",
    },
  },
});
