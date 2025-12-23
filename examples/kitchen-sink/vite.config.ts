import { defineConfig } from "vitest/config";

export default defineConfig({
  server: {
    open: false,
  },
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
