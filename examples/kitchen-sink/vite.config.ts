import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
  server: {
    // Makes the custom route from `src/serverSetup.ts` reachable on the app URL in development
    proxy: {
      ...(process.env.WASP_DEV_PROXY_TARGET && {
        "/customRoute": process.env.WASP_DEV_PROXY_TARGET,
      }),
    },
  },
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
