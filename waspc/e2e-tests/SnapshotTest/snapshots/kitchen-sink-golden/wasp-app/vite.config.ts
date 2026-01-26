import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";

export default defineConfig({
  server: {
    open: false,
  },
  plugins: [tailwindcss()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
