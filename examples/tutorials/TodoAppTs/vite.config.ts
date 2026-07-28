import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
