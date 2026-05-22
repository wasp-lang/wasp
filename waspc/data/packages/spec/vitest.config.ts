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
