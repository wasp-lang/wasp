import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { resolve } from "node:path";
import { defineConfig } from "vite";

// Builds the SPA into `dist/client`, which the node side serves. Relative asset
// URLs let devframe serve it from any base path.
export default defineConfig({
  base: "./",
  root: import.meta.dirname,
  plugins: [react(), tailwindcss()],
  build: {
    outDir: resolve(import.meta.dirname, "../../dist/client"),
    emptyOutDir: true,
  },
});
