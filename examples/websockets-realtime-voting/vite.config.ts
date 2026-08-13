import tailwindcss from "@tailwindcss/vite";
import flowbiteReact from "flowbite-react/plugin/vite";
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";
import { waspServer } from "wasp/server/vite";

export default defineConfig({
  plugins: [wasp(), waspServer(), tailwindcss(), flowbiteReact()],
  test: {
    exclude: ["./e2e-tests/**"],
  },
});
