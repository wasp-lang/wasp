import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";
import { waspServer } from "wasp/server/vite";

export default defineConfig({
  plugins: [wasp(), waspServer(), tailwindcss()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
