import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";
import { waspTestConfig } from "wasp/test";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  test: waspTestConfig({
    setupFiles: ["./src/test/setup.ts"],
  }),
});
