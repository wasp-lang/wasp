/// <reference types="vitest/config" />
import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vite";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    open: false,
  },
  plugins: [wasp(), tailwindcss()],
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: ["./src/test/setup.ts"],
    exclude: ["./e2e-tests/**", "**/node_modules/**", "**/.wasp/**"],
    env: {
      DATABASE_URL: "postgresql://localhost/test",
    },
  },
});
