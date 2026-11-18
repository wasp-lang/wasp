import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    projects: [
      {
        test: {
          name: "unit",
          include: ["__tests__/**/*.unit.test.ts"],
        },
      },
      {
        test: {
          name: "integration",
          include: ["__tests__/**/*.integration.test.ts"],
          // The spec-pipeline tests build the package and run its CLI in a
          // subprocess (npm i + npx), which can take well over the default 5s
          // timeout on slower CI runners.
          testTimeout: 120_000,
          hookTimeout: 120_000,
        },
      },
      {
        test: {
          name: "type",
          typecheck: {
            enabled: true,
            only: true,
            include: ["__tests__/**/*.test-d.ts"],
          },
        },
      },
    ],
  },
});
