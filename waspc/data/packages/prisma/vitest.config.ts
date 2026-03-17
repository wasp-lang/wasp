import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    env: {
      // Prevents Prisma from printing upgrade notices to stderr,
      // which would cause test assertions on stderr to fail.
      PRISMA_HIDE_UPDATE_MESSAGE: "true",
    },
  },
});
