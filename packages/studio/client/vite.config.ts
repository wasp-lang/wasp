import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: {
    // Same port as `wasp studio`, which the app's server lets read its spec.
    port: 4000,
    strictPort: true,
  },
});
