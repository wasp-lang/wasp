import tailwindcss from "@tailwindcss/vite";
import { defineConfig } from "vite";

export default defineConfig({
  server: {
    open: true,
  },
  plugins: [tailwindcss()],
});
