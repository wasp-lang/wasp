import tailwindcss from "@tailwindcss/vite";
import flowbiteReact from "flowbite-react/plugin/vite";
import { defineConfig } from "vite";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  server: {
    open: true,
  },
  plugins: [wasp(), tailwindcss(), flowbiteReact()],
});
