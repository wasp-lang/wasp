import tailwindcss from "@tailwindcss/vite";
import { wasp } from "@wasp/wasp/vite";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [wasp(), tailwindcss()],
});
