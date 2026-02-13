import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    // Keep this option to 4000, this makes sure our Vite option
    // forcing works since e2e tests rely on client being served
    // on port 3000.
    port: 4000,
    open: false,
  },
  plugins: [wasp(), tailwindcss()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
