import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    open: false,
  },
  plugins: [wasp(), tailwindcss()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
  build: { minify: false },
  resolve: {
    alias: {
      "react-dom/client": "react-dom/profiling",
    },
  },
});
