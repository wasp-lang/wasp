import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { waspTestConfig } from "wasp/client/test/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  test: waspTestConfig({
    setupFiles: ["./src/test/setup.ts"],
  }),
});
