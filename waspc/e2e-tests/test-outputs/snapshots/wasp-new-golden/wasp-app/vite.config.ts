import { defineConfig } from "vite";
import { wasp } from "@wasp/wasp/vite";

export default defineConfig({
  plugins: [wasp()],
});
