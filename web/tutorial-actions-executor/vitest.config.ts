import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["tests/**/*.test.ts", "e2e-tests/**/*.test.ts"],
    // `e2e-tests/.result` holds the generated app, which ships its own tests.
    exclude: ["**/node_modules/**", "**/.result/**"],
    environment: "node",
  },
});
