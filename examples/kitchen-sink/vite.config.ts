import { defaultExclude, defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp()],
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: ["./src/vitest/setup.ts"],
    exclude: [...defaultExclude, "./e2e-tests/**/*"],
  },
});
