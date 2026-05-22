import { configDefaults, defineConfig } from "vitest/config";

const testExcludes = [...configDefaults.exclude, "**/dist/**"];

export default defineConfig({
  test: {
    dir: "__tests__",
    exclude: testExcludes,
    projects: [
      {
        test: {
          name: "unit",
          include: ["**/*.unit.test.ts"],
        },
      },
      {
        test: {
          name: "integration",
          include: ["**/*.integration.test.ts"],
        },
      },
      {
        test: {
          name: "type",
          include: ["**/*.test-d.ts"],
          typecheck: {
            enabled: true,
            only: true,
            include: ["__tests__/**/*.test-d.ts"],
            exclude: testExcludes,
          },
        },
      },
    ],
  },
});
