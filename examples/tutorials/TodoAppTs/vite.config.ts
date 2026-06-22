import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp()],
  server: {
    open: true,
  },
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
