import tailwindcss from "@tailwindcss/vite";
import { waspTestConfig } from "@wasp/test-utils/config";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  test: waspTestConfig({
    setupFiles: ["./src/test/setup.ts"],
  }),
});
