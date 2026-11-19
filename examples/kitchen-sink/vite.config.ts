import tailwindcss from "@tailwindcss/vite";
import { waspTestConfig } from "@wasp/wasp/test";
import { wasp } from "@wasp/wasp/vite";
import { defineConfig } from "vitest/config";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  test: waspTestConfig({
    setupFiles: ["./src/test/setup.ts"],
  }),
});
