import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp, waspDevTools } from "wasp/client/vite";
import { waspTestConfig } from "wasp/test";

export default defineConfig({
  devtools: waspDevTools(),
  plugins: [wasp(), tailwindcss()],
  test: waspTestConfig({
    setupFiles: ["./src/test/setup.ts"],
  }),
});
