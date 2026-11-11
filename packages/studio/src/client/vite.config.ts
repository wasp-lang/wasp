import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vite";

// Builds the SPA into `dist/client`, which the node side serves. Relative asset
// URLs let devframe serve it from any base path.
export default defineConfig({
  base: "./",
  root: fileURLToPath(new URL(".", import.meta.url)),
  plugins: [react(), tailwindcss()],
  build: {
    outDir: fileURLToPath(new URL("../../dist/client", import.meta.url)),
    emptyOutDir: true,
  },
});
