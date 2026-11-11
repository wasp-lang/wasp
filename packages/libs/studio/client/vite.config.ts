import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { resolve } from "node:path";
import { defineConfig } from "vite";

export default defineConfig({
  root: import.meta.dirname,
  plugins: [react(), tailwindcss()],
  // DevTools serves the panel under its own path, so the assets are relative.
  base: "./",
  build: {
    outDir: resolve(import.meta.dirname, "../dist/client"),
    emptyOutDir: true,
  },
});
