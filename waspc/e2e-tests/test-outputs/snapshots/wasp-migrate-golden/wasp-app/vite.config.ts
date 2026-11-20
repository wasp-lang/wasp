import { defineConfig } from "vite";
import { wasp, waspDevTools } from "wasp/client/vite";

export default defineConfig({
  devtools: waspDevTools(),
  plugins: [wasp()],
});
