import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), tailwindcss()],
  // Relative, so the page works wherever it's served: at the root by
  // `wasp studio`, and under a path by the Vite DevTools panel.
  base: "./",
});
