import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [react(), tailwindcss()],
  // DevTools serves the panel under its own path, so the assets are relative.
  base: "./",
  build: {
    outDir: "../dist/client",
    emptyOutDir: true,
  },
});
