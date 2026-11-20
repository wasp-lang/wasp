import tailwindcss from "@tailwindcss/vite";
import flowbiteReact from "flowbite-react/plugin/vite";
import { defineConfig } from "vite";
import { wasp, waspDevTools } from "wasp/client/vite";

export default defineConfig({
  devtools: waspDevTools(),
  plugins: [wasp(), tailwindcss(), flowbiteReact()],
});
