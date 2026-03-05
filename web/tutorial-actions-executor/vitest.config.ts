import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["tests/**/*.test.ts", "e2e-tests/**/*.test.ts"],
    environment: "node",
  },
});
