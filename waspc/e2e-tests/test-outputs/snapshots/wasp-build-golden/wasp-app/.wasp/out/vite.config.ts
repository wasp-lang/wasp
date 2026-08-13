import { defineConfig } from "vite";
import { wasp } from "wasp/client/vite";
import { waspServer } from "wasp/server/vite";

export default defineConfig({
  plugins: [wasp(), waspServer()],
});
