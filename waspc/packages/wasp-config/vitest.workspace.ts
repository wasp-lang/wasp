import { defineWorkspace } from "vitest/config";

export default defineWorkspace([
  {
    test: {
      name: "unit",
      include: ["**/__tests__/**/*.unit.test.ts"],
    },
  },
  {
    test: {
      name: "integration",
      include: ["**/__tests__/**/*.integration.test.ts"],
    },
  },
  {
    test: {
      name: "type",
      include: ["**/__tests__/**/*.test-d.ts"],
    },
  },
]);
